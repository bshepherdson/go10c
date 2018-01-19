package main

import "log"

func (l *Location) AsDArg() DArg {
	switch l.Kind {
	case locReg:
		return daReg(l.n)
	case locConstant:
		return daLit(l.n)
	case locFrame:
		return &DAFrameOffset{delta: l.n}
	case locLabel:
		return &DALabel{label: l.label, indirect: true}
	}
	log.Fatalf("Can't happen: Unknown location kind %d", l.Kind)
	return nil
}

func (p *Package) Gen(c *Compiler) {
	for _, d := range p.Decls {
		d.Gen(c)
	}
}

// Top-level global variable declaration.
func (vd *VarDecl) Gen(c *Compiler) {
	c.this = ""
	l := c.namedLabel("global_" + vd.Name)
	c.globals[vd.Name] = label(l)

	c.compile(dLabel(l))
	size := vd.Type.Size()
	if size == 1 && (vd.Value == nil || vd.Value.AsDArg(c).Static()) {
		// Size 1 and fits in an arg, so it's fine to use a .dat
		arg := daLit(0)
		if vd.Value != nil {
			arg = vd.Value.AsDArg(c)
		}
		c.compile(dUnary(".dat", arg))
	} else {
		c.compile(&DReserve{size: vd.Type.Size()})

		if vd.Value != nil {
			c.initialization = true
			loc := &DExprResult{}
			vd.Value.GenExpr(c, loc)

			c.initialization = false
		}
	}
}

func (fd *FuncDecl) Gen(c *Compiler) {
	c.this = fd.Name
	funcLabel := c.namedLabel("")

	c.compile(dLabel(funcLabel))

	var nextArg *Data = reg(0) // First arg is in A.
	argCount := 0
	// Add the receiver to the scope.
	if fd.Receiver != nil && len(fd.Receiver.Names) > 0 {
		c.newData(fd.Receiver.Names[0], nextArg.loc)
		nextArg = reg(1)
		argCount++
	}

	for _, spec := range fd.Args {
		for _, name := range spec.Names {
			argCount++
			c.newData(name, nextArg.loc)
			if nextArg.loc.Kind == locReg {
				if nextArg.loc.n == 2 { // Move to using the stack, starting at FP+1.
					nextArg = frame(1)
				} else {
					nextArg = reg(nextArg.loc.n + 1)
				}
			} else if nextArg.loc.Kind == locFrame {
				nextArg = frame(nextArg.loc.n + 1)
			}
		}
	}

	// Now we need to either collect locals or save the position and update it
	// later. I think it would make my life easier to save space for a preamble.
	oldCode := c.code
	c.code = []Outputtable{}
	c.localCount = 0

	toReserve := argCount
	if argCount > 3 {
		toReserve = 3
	}
	c.alloc = newAllocator(toReserve)

	for _, stmt := range fd.Body {
		stmt.Gen(c)
	}

	myCode := c.code
	c.code = oldCode

	// Compile the preamble: push the old FP, set FP to that spot, move SP down
	// far enough to make room for my locals.
	c.compile(dPush(daFP()))
	c.compile(dBin("set", daFP(), daSP()))
	c.compile(dBin("sub", daSP(), daLit(c.localCount)))

	// Now save X, Y, I and J, as needed.
	if c.alloc.peak > 5 {
		c.alloc.peak++ // Lie and say it's one higher, to account for Z.
	}

	for i := 3; i < 8; i++ {
		if i == 5 { // Skip over Z.
			continue
		}
		if c.alloc.peak > i {
			c.compile(dPush(daReg(i)))
		}
	}

	// Concat in the function's own code.
	c.code = append(c.code, myCode...)

	// Append the postamble: exit label, pop registers, load old FP, return.
	c.compile(dLabel(c.namedLabel("end")))
	for i := 7; i >= 3; i-- {
		if i == 5 {
			continue
		}
		if c.alloc.peak > i {
			c.compile(dPop(daReg(i)))
		}
	}

	indirectFP := daFP()
	indirectFP.(*DAReg).indirect = true
	c.compile(dBin("set", daSP(), daFP()))
	c.compile(dPop(daFP()))
	c.compile(dPop(daPC()))
}

func (td *TypeDecl) Gen(c *Compiler) {
	// Nothing to do; types don't exist at runtime.
}

/*
// An assignment or short declaration.
type AssignStmt struct {
	Pos token.Pos
	Op  token.Token // assignment token, or DEFINE
	Lhs []Expr
	Rhs []Expr
}
*/
func (s *AssignStmt) Gen(c *Compiler) {
	for i, r := range s.Rhs {
		l := s.Lhs[i]

		tgt := &DExprResult{}
		r.GenExpr(c, tgt)

		if s.Op == token.DEFINE {
			// If this is a short declaration, create a new local for it.
			c.localCount++
			c.data.
			// START HERE: Handle the different operations, not just =.
			// Also, the data map is a bit too unsophisticated; it can't handle
			// shadowing variable names. That needs fixing first.


		// Store the result into the LHS. Depends on what was on the left.
		var loc DArg
		switch lx := l.(type) {
		case *Ident:
			loc = c.data[lx.Name].loc.AsDArg()

		case *IndexExpr:
			loc = dArraySlot(c, lx)

		case *SelectorExpr:
			loc = dSelectorSlot(c, lx)

		default:
			log.Fatal("Can't happen: Illegal lvalue; typechecking is busted.")
		}

		// We now have our value and destination, suitable for use an an lvalue.
		c.compile(dBin("set", loc, tgt.result))
	}
}

// Forces the given argument to be usable as a left-hand side.
func forceLHS(c *Compiler, arg DArg) DArg {
	switch a := arg.(type) {
	case *DAReg, *DASpecial, *DARegLit, *DAFrameOffset:
		return a // These ones are already good.
	case *DALit:
		if a.indirect {
			return a // That's good too.
		}
		// Needs to be loaded in.
		reg := c.alloc.Get()
		out := &DAReg{reg: reg}
		c.compile(dBin("set", out, arg))
		return out
	default:
		log.Fatal("Unknown type in forceLHS")
		return nil
	}
}

func dArraySlot(c *Compiler, x *IndexExpr) DArg {
	// First solve the index expression to get the index.
	idx := &DExprResult{}
	x.Index.GenExpr(c, idx)
	// Then the base expression to get the array head.
	arr := &DExprResult{}
	x.Expr.GenExpr(c, arr)
	arr.result = forceLHS(c, arr.result)

	// If the array element size is not 1, multiply the index by it.
	elementSize := x.Expr.TypeOf(c).(*ArrayType).Inner.Size()
	if elementSize != 1 {
		idx.result = forceLHS(c, idx.result)
		c.compile(dBin("mul", idx.result, daLit(elementSize)))
	}

	// Add the offset to the array start to get the address of storage.
	c.compile(dBin("add", arr.result, idx.result))

	// And finally set the result into it.
	arr.result.(*DAReg).indirect = true
	return arr.result
}

func dSelectorSlot(c *Compiler, x *SelectorExpr) DArg {
	// x.Expr must be either a pointer to a struct, or a struct.
	// We work out the offset and then build the argument accordingly.
	// In the best case, x.Expr is a constant pointer, and we can provide an
	// adjusted constant pointer.
	// In the normal case, x.Expr is some computed value, which we need to read
	// into a register and maybe dereference.

	lhs := &DExprResult{}
	x.Expr.GenExpr(c, lhs)
	t := x.Expr.TypeOf(c)
	isPointer := false
	if pt, ok := t.(*PointerType); ok {
		isPointer = true
		t = pt.Inner
	}

	st, ok := t.(*StructType)
	if !ok {
		log.Fatalf("Can't happen: typechecker let a non-struct through")
	}

	offset := 0

fieldLoop:
	for _, field := range st.Fields {
		for _, name := range field.Names {
			if name == x.Selector {
				break fieldLoop
			}
			offset += field.Type.Size()
		}
	}

	// A "raw" StructType is represented at runtime as a pointer to the start of
	// the block. A PointerType to it is doubly indirect.
	// Having evaluated it above, we now have either the pointer or double pointer
	// to the struct ready.
	// What we want to return is a pointer directly to the value itself.
	// If we have a double pointer, we need to dereference it first.
	//
	// Cases for where the raw struct's address might be:
	// Raw struct at Lit, no indirect: Return adjusted Lit, indirected.
	// Raw struct in Reg, no indirect: Return RegLit.
	// Elsewhere: deref to reg, return RegLit.

	structRef := lhs.result
	if isPointer {
		structRef = daReg(deref(c, structRef))
	}

	switch loc := structRef.(type) {
	case *DALit:
		if !loc.indirect {
			return &DALit{lit: loc.lit + offset, indirect: true}
		}
	case *DAReg:
		if !loc.indirect {
			return &DARegLit{reg: loc.reg, lit: offset}
		}
	}

	// If those didn't hit it, we need to dereference whatever argument we got.
	r := deref(c, structRef)
	return &DARegLit{reg: r, lit: offset}
}

// Returns a register number where the value has been dereferenced.
// The name is a bit of a misnomer. More to the point, this forces the input
// argument into a register.
func deref(c *Compiler, ref DArg) int {
	r := c.alloc.Get()
	c.compile(dBin("set", daReg(r), ref))
	return r
}

/*
// A bare block.
type BlockStmt struct {
	Pos  token.Pos
	Body []Statement
}
*/
func (s *BlockStmt) Gen(c *Compiler) {
	// TODO
}

/*
// A nested set of statements - no new scope.
type NestedStmt struct {
	Stmts []Statement
}
*/
func (s *NestedStmt) Gen(c *Compiler) {
	for _, stmt := range s.Stmts {
		stmt.Gen(c)
	}
}

/*
// Control flow statements: break, continue, goto and fallthrough.
type BranchStmt struct {
	Pos     token.Pos
	Flavour token.Token
	Label   string // Target label, if any.
}
*/
func (s *BranchStmt) Gen(c *Compiler) {
	// TODO
}

/*
type DeclStmt struct {
	Pos  token.Pos
	Decl *VarDecl
}
*/
func (s *DeclStmt) Gen(c *Compiler) {
	// TODO
}

/*
// An expression on its own line; generally this is a call.
type ExprStmt struct {
	Pos  token.Pos
	Expr Expr
}
*/
func (s *ExprStmt) Gen(c *Compiler) {
	// TODO
}

/*
type ForStmt struct {
	Pos  token.Pos
	Init Statement
	Cond Expr
	Post Statement
	Body []Statement
}
*/
func (s *ForStmt) Gen(c *Compiler) {
	// TODO
}

/*
type IfStmt struct {
	Pos  token.Pos
	Init Statement
	Cond Expr
	Body []Statement
	Else Statement // Or nil
}
*/
func (s *IfStmt) Gen(c *Compiler) {
	// TODO
}

/*
type IncDecStmt struct {
	Pos  token.Pos
	Expr Expr
	Op   token.Token
}
*/
func (s *IncDecStmt) Gen(c *Compiler) {
	// TODO
}

/*
type LabeledStmt struct {
	Pos   token.Pos
	Label string
	Stmt  Statement
}
*/
func (s *LabeledStmt) Gen(c *Compiler) {
	// TODO
}

/*
type ReturnStmt struct {
	Pos     token.Pos
	Results []Expr
}
*/
func (s *ReturnStmt) Gen(c *Compiler) {
	// TODO
}

/*
type Ident struct {
	Pos  token.Pos
	Name string
}
*/
func (x *Ident) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *Ident) AsDArg(c *Compiler) DArg {
	d := c.locate(x.Name)
	return d.loc.AsDArg()
}

/*
type NumLit struct {
	Pos   token.Pos
	Value int
}
*/
func (x *NumLit) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *NumLit) AsDArg(c *Compiler) DArg {
	return nil
}

/*
type CharLit struct {
	Pos   token.Pos
	Value rune
}
*/
func (x *CharLit) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *CharLit) AsDArg(c *Compiler) DArg {
	return nil
}

/*
type StringLit struct {
	Pos   token.Pos
	Value string
}
*/
func (x *StringLit) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *StringLit) AsDArg(c *Compiler) DArg {
	return nil
}

/*
type UnaryExpr struct {
	Pos  token.Pos
	Op   token.Token
	Expr Expr
}
*/
func (x *UnaryExpr) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *UnaryExpr) AsDArg(c *Compiler) DArg {
	return nil
}

/*
type BinaryExpr struct {
	Pos token.Pos
	Lhs Expr
	Op  token.Token
	Rhs Expr
}
*/
func (x *BinaryExpr) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *BinaryExpr) AsDArg(c *Compiler) DArg {
	return nil
}

/*
type CallExpr struct {
	Pos  token.Pos
	Fun  Expr
	Args []Expr
}
*/
func (x *CallExpr) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *CallExpr) AsDArg(c *Compiler) DArg {
	return nil
}

// foo[bar]
/*
type IndexExpr struct {
	Pos   token.Pos
	Expr  Expr
	Index Expr
}
*/
func (x *IndexExpr) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *IndexExpr) AsDArg(c *Compiler) DArg {
	return nil
}

// foo.bar
/*
type SelectorExpr struct {
	Pos      token.Pos
	Expr     Expr
	Selector string
}
*/
func (x *SelectorExpr) GenExpr(c *Compiler, res Target) {
	// TODO
}

func (x *SelectorExpr) AsDArg(c *Compiler) DArg {
	return nil
}
