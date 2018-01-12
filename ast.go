package main

import (
	"go/ast"
	"go/token"
	"log"
	"strconv"
)

// Since we cannot declare methods on foreign types, we follow parsing with a
// processing pass where we use ast.Walk to comb the tree and convert everything
// into our local formats.

type Located interface {
	Loc() token.Pos
}

type AST interface {
	Located
	TypeCheck(*Compiler)
}

type Package struct {
	Pos   token.Pos
	Name  string
	Decls []Decl
}

// A declaration - of a type, function or variable.
type Decl interface {
	AST
	CollectSymbols(*SymbolTable)
}

// (Names, Type) pair for variable declarations, function args, struct fields.
type VarSpec struct {
	Names []string
	Type  Type
}

// Actual declaration of a new variable, with optional initial value.
type VarDecl struct {
	Pos   token.Pos
	Name  string
	Type  Type
	Value Expr
}

type FuncDecl struct {
	Pos        token.Pos
	Name       string
	Receiver   *VarSpec
	Args       []*VarSpec
	ReturnType Type
	Body       []Statement
}

// A (name, type) pair that gives a new name to a type.
type TypeDecl struct {
	Pos  token.Pos
	Name string
	Type Type
}

type Statement interface {
	AST
}

// An assignment or short declaration.
type AssignStmt struct {
	Pos token.Pos
	Op  token.Token // assignment token, or DEFINE
	Lhs []Expr
	Rhs []Expr
}

// A bare block.
type BlockStmt struct {
	Pos  token.Pos
	Body []Statement
}

// A nested set of statements - no new scope.
type NestedStmt struct {
	Stmts []Statement
}

// Control flow statements: break, continue, goto and fallthrough.
type BranchStmt struct {
	Pos     token.Pos
	Flavour token.Token
	Label   string // Target label, if any.
}

type DeclStmt struct {
	Pos  token.Pos
	Decl *VarDecl
}

// An expression on its own line; generally this is a call.
type ExprStmt struct {
	Pos  token.Pos
	Expr Expr
}

type ForStmt struct {
	Pos  token.Pos
	Init Statement
	Cond Expr
	Post Statement
	Body []Statement
}

type IfStmt struct {
	Pos  token.Pos
	Init Statement
	Cond Expr
	Body []Statement
	Else Statement // Or nil
}

type IncDecStmt struct {
	Pos  token.Pos
	Expr Expr
	Op   token.Token
}

type LabeledStmt struct {
	Pos   token.Pos
	Label string
	Stmt  Statement
}

type ReturnStmt struct {
	Pos     token.Pos
	Results []Expr
}

// TODO: Select

type Expr interface {
	Located
	TypeOf(*Compiler) Type
	Lvalue() bool // Indicates that this expression can be written to.
}

type Ident struct {
	Pos  token.Pos
	Name string
}

type NumLit struct {
	Pos   token.Pos
	Value int
}

type CharLit struct {
	Pos   token.Pos
	Value rune
}

type StringLit struct {
	Pos   token.Pos
	Value string
}

type UnaryExpr struct {
	Pos  token.Pos
	Op   token.Token
	Expr Expr
}

type BinaryExpr struct {
	Pos token.Pos
	Lhs Expr
	Op  token.Token
	Rhs Expr
}

type CallExpr struct {
	Pos  token.Pos
	Fun  Expr
	Args []Expr
}

// foo[bar]
type IndexExpr struct {
	Pos   token.Pos
	Expr  Expr
	Index Expr
}

// foo.bar
type SelectorExpr struct {
	Pos      token.Pos
	Expr     Expr
	Selector string
}

func astPackage(f *ast.File, c *Compiler) *Package {
	p := &Package{Pos: f.Package, Name: f.Name.Name}
	for _, d := range f.Decls {
		d2 := astDecl(d, c)
		if d2 != nil {
			p.Decls = append(p.Decls, d2...)
		}
	}
	return p
}

// ast.Decl is eg. a var (...) block, with an []Spec, where Spec is ImportSpec,
// ValueSpec, TypeSpec or ValueSpec.
// Or it might be an ast.FuncDecl.
func astDecl(di ast.Decl, c *Compiler) []Decl {
	switch d := di.(type) {
	case *ast.GenDecl:
		switch d.Tok {
		case token.TYPE:
			decls := []Decl{}
			for _, spec := range d.Specs {
				if ts, ok := spec.(*ast.TypeSpec); ok {
					// ts.Name, ts.Type
					td := &TypeDecl{Pos: d.TokPos, Name: ts.Name.Name, Type: astType(ts.Type, c)}
					decls = append(decls, td)
				} else {
					log.Fatalf("Can't happen, token.TYPE but not TypeSpec")
				}
			}
			return decls
		case token.VAR:
			decls := []Decl{}
			for _, spec := range d.Specs {
				if ts, ok := spec.(*ast.ValueSpec); ok {
					// ts.Names, ts.Type, ts.Values
					t := astType(ts.Type, c)
					for i, ident := range ts.Names {
						vd := &VarDecl{Pos: ts.Pos(), Name: ident.Name, Type: t}
						if ts.Values != nil && len(ts.Values) > 0 {
							vd.Value = astExpr(ts.Values[i])
						}
						decls = append(decls, vd)
					}
				} else {
					log.Fatalf("Can't happen, token.TYPE but not TypeSpec")
				}
			}
			return decls
		}

	case *ast.FuncDecl:
		f := &FuncDecl{Pos: d.Pos(), Name: d.Name.Name}
		if d.Recv != nil && len(d.Recv.List) > 0 {
			field := d.Recv.List[0]
			f.Receiver = astField(field, c)
		}

		for _, field := range d.Type.Params.List {
			f.Args = append(f.Args, astField(field, c))
		}

		for _, stmt := range d.Body.List {
			f.Body = append(f.Body, astStmt(stmt, c))
		}

		if d.Type.Results != nil {
			if len(d.Type.Results.List) > 1 {
				log.Fatalf("Multiple return values are not supported")
			} else if len(d.Type.Results.List) == 1 {
				f.ReturnType = astType(d.Type.Results.List[0].Type, c)
			}
		}

		return []Decl{f}
	}

	log.Fatalf("Can't happen: bottom of astDecl")
	return []Decl{}
}

func astField(f *ast.Field, c *Compiler) *VarSpec {
	v := &VarSpec{Type: astType(f.Type, c)}
	for _, name := range f.Names {
		v.Names = append(v.Names, name.Name)
	}
	return v
}

func astType(ti ast.Expr, c *Compiler) Type {
	switch t := ti.(type) {
	case *ast.ArrayType:
		// We don't support lengths, so we can ignore that clause.
		return &ArrayType{Inner: astType(t.Elt, c)}

	case *ast.FuncType:
		if len(t.Results.List) > 1 || (len(t.Results.List) == 1 && len(t.Results.List[0].Names) > 1) {
			log.Fatalf("Multiple return values are not supported")
		}
		theType := &FuncType{}
		for _, f := range t.Params.List {
			t2 := astType(f.Type, c)
			for range f.Names {
				theType.Args = append(theType.Args, t2)
			}
		}

		if len(t.Results.List) == 1 {
			if t.Results.List[0].Names[0] != nil {
				log.Fatalf("Named return values are not supported")
			}
			theType.ReturnType = astType(t.Results.List[0].Type, c)
		}
		return theType

	case *ast.StructType:
		st := &StructType{}
		if t.Fields == nil || t.Fields.List == nil || len(t.Fields.List) == 0 {
			return st
		}

		for _, f := range t.Fields.List {
			vs := &VarSpec{Type: astType(f.Type, c)}
			for _, n := range f.Names {
				vs.Names = append(vs.Names, n.Name)
			}
			st.Fields = append(st.Fields, vs)
		}

		return st

	case *ast.StarExpr: // Pointer to another type.
		return &PointerType{Inner: astType(t.X, c)}

	case *ast.Ident:
		if t.Name == "int" || t.Name == "uint" || t.Name == "char" || t.Name == "bool" || t.Name == "string" {
			return BuiltinType(t.Name)
		}
		return &NamedType{Name: t.Name, Compiler: c}
	}

	// This return nil is actually legal, for a nil Type.
	// That can happen with var foo = bar; the engine should infer.
	return nil
}

func astStmt(iStmt ast.Stmt, c *Compiler) Statement {
	switch stmt := iStmt.(type) {
	case *ast.AssignStmt:
		return astAssignStmt(stmt)

	case *ast.ExprStmt:
		return &ExprStmt{Pos: stmt.Pos(), Expr: astExpr(stmt.X)}

	case *ast.IncDecStmt:
		return &IncDecStmt{Pos: stmt.TokPos, Op: stmt.Tok, Expr: astExpr(stmt.X)}

	case *ast.DeclStmt:
		d := astDecl(stmt.Decl, c)
		ret := &NestedStmt{}
		for _, vdi := range d {
			if vd, ok := vdi.(*VarDecl); ok {
				ret.Stmts = append(ret.Stmts, &DeclStmt{Pos: vd.Pos, Decl: vd})
			} else {
				c.typeError(vdi, "Cannot declare functions or types inside functions.")
			}
		}
		return ret

	case *ast.LabeledStmt:
		return &LabeledStmt{Pos: stmt.Colon, Label: stmt.Label.Name, Stmt: astStmt(stmt.Stmt, c)}

	case *ast.BranchStmt:
		b := &BranchStmt{Pos: stmt.TokPos, Flavour: stmt.Tok}
		if stmt.Label != nil {
			b.Label = stmt.Label.Name
		}
		return b

	case *ast.BlockStmt:
		b := &BlockStmt{Pos: stmt.Lbrace}
		for _, s := range stmt.List {
			b.Body = append(b.Body, astStmt(s, c))
		}
		return b

	case *ast.IfStmt:
		return astIfStmt(stmt, c)

	case *ast.ForStmt:
		return astForStmt(stmt, c)

	case *ast.ReturnStmt:
		r := &ReturnStmt{Pos: stmt.Return}
		for _, expr := range stmt.Results {
			r.Results = append(r.Results, astExpr(expr))
		}
		return r
	}
	return nil
}

func astIfStmt(stmt *ast.IfStmt, c *Compiler) *IfStmt {
	ret := &IfStmt{Pos: stmt.If}
	if stmt.Init != nil {
		ret.Init = astStmt(stmt.Init, c)
	}
	ret.Cond = astExpr(stmt.Cond)
	for _, b := range stmt.Body.List {
		ret.Body = append(ret.Body, astStmt(b, c))
	}

	if stmt.Else != nil {
		ret.Else = astStmt(stmt.Else, c)
	}
	return ret
}

func astForStmt(stmt *ast.ForStmt, c *Compiler) *ForStmt {
	ret := &ForStmt{Pos: stmt.For}
	if stmt.Init != nil {
		ret.Init = astStmt(stmt.Init, c)
	}
	if stmt.Cond != nil {
		ret.Cond = astExpr(stmt.Cond)
	}
	if stmt.Post != nil {
		ret.Post = astStmt(stmt.Post, c)
	}

	for _, b := range stmt.Body.List {
		ret.Body = append(ret.Body, astStmt(b, c))
	}

	return ret
}

func astAssignStmt(stmt *ast.AssignStmt) *AssignStmt {
	a := &AssignStmt{Pos: stmt.TokPos, Op: stmt.Tok}
	for _, lhs := range stmt.Lhs {
		a.Lhs = append(a.Lhs, astExpr(lhs))
	}
	for _, rhs := range stmt.Rhs {
		a.Rhs = append(a.Rhs, astExpr(rhs))
	}
	return a
}

func astExpr(expr ast.Expr) Expr {
	switch x := expr.(type) {

	case *ast.BasicLit:
		switch x.Kind {
		case token.INT:
			n := &NumLit{Pos: x.ValuePos}
			i, _ := strconv.ParseInt(x.Value, 0, 64)
			n.Value = int(i)
			return n
		case token.STRING:
			s := &StringLit{Pos: x.ValuePos}
			if x.Value[0] != '"' {
				log.Fatalf("String literal must start with \", got %c", x.Value[0])
			}
			s.Value = x.Value[1 : len(x.Value)-1]
			return s
		case token.CHAR:
			return &CharLit{Pos: x.ValuePos, Value: rune(x.Value[1])}
		}

	case *ast.UnaryExpr:
		return &UnaryExpr{Pos: x.OpPos, Op: x.Op, Expr: astExpr(x.X)}

	case *ast.BinaryExpr:
		return &BinaryExpr{Pos: x.OpPos, Lhs: astExpr(x.X), Op: x.Op, Rhs: astExpr(x.Y)}

	case *ast.CallExpr:
		e := &CallExpr{Pos: x.Lparen, Fun: astExpr(x.Fun)}
		if x.Ellipsis != token.NoPos {
			log.Fatalf("Ellipsis (...) in functions is not supported")
		}
		for _, a := range x.Args {
			e.Args = append(e.Args, astExpr(a))
		}
		return e

	case *ast.IndexExpr:
		return &IndexExpr{Pos: x.Lbrack, Expr: astExpr(x.X), Index: astExpr(x.Index)}

	case *ast.SelectorExpr:
		return &SelectorExpr{Pos: x.Sel.Pos(), Expr: astExpr(x.X), Selector: x.Sel.Name}

	case *ast.ParenExpr:
		return astExpr(x.X)

	case *ast.StarExpr:
		return &UnaryExpr{Pos: x.Star, Op: token.MUL, Expr: astExpr(x.X)}

	case *ast.Ident:
		return astIdent(x)
	}
	return nil // TODO
}

func astIdent(expr *ast.Ident) *Ident {
	return &Ident{Pos: expr.NamePos, Name: expr.Name}
}

func (a *Package) Loc() token.Pos     { return a.Pos }
func (a *VarDecl) Loc() token.Pos     { return a.Pos }
func (a *FuncDecl) Loc() token.Pos    { return a.Pos }
func (a *TypeDecl) Loc() token.Pos    { return a.Pos }
func (a *AssignStmt) Loc() token.Pos  { return a.Pos }
func (a *ReturnStmt) Loc() token.Pos  { return a.Pos }
func (a *ExprStmt) Loc() token.Pos    { return a.Pos }
func (a *IncDecStmt) Loc() token.Pos  { return a.Pos }
func (a *DeclStmt) Loc() token.Pos    { return a.Pos }
func (a *LabeledStmt) Loc() token.Pos { return a.Pos }
func (a *NestedStmt) Loc() token.Pos  { return a.Stmts[0].Loc() }
func (a *BranchStmt) Loc() token.Pos  { return a.Pos }
func (a *BlockStmt) Loc() token.Pos   { return a.Pos }
func (a *IfStmt) Loc() token.Pos      { return a.Pos }
func (a *ForStmt) Loc() token.Pos     { return a.Pos }

func (a *Ident) Loc() token.Pos        { return a.Pos }
func (a *NumLit) Loc() token.Pos       { return a.Pos }
func (a *CharLit) Loc() token.Pos      { return a.Pos }
func (a *StringLit) Loc() token.Pos    { return a.Pos }
func (a *UnaryExpr) Loc() token.Pos    { return a.Pos }
func (a *BinaryExpr) Loc() token.Pos   { return a.Pos }
func (a *CallExpr) Loc() token.Pos     { return a.Pos }
func (a *IndexExpr) Loc() token.Pos    { return a.Pos }
func (a *SelectorExpr) Loc() token.Pos { return a.Pos }

func (x *Ident) Lvalue() bool        { return true }
func (a *IndexExpr) Lvalue() bool    { return true }
func (a *SelectorExpr) Lvalue() bool { return true }
func (x *NumLit) Lvalue() bool       { return false }
func (x *CharLit) Lvalue() bool      { return false }
func (x *StringLit) Lvalue() bool    { return false }
func (x *BinaryExpr) Lvalue() bool   { return false }
func (x *CallExpr) Lvalue() bool     { return false }
func (x *UnaryExpr) Lvalue() bool {
	return x.Op == token.MUL // Only *foo is an Lvalue.
}
