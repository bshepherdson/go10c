package main

import "log"

type Checkable interface {
	TypeCheck(*Compiler) *Type
}

type Typed interface {
	TypeOf(*Compiler) *Type
}

func (c *Compiler) typeCheckAll(pkg *GoFile) {
	c.symbols = c.packages[pkg.Package].symbols
	c.types = c.packages[pkg.Package].types

	for _, d := range pkg.Decls {
		if d.Func != nil {
			d.Func.TypeCheck(c)
		} else {
			s := d.Simple
			if s.Var != nil {
				// TODO: Handle variables with top-level values.
			}
		}
	}
}

func (f *FuncDecl) TypeCheck(c *Compiler) {
	c.this = f.Name
	originalTable := c.symbols
	c.symbols = newSymbolTable(c.symbols)

	for _, argGroup := range f.Args {
		for _, name := range argGroup.Args {
			c.symbols.add(name, argGroup.Type)
		}
	}

	// Now work the compiler through each statement in the body.
	for _, stmt := range f.Body {
		stmt.TypeCheck(c)
	}

	// If we got to here, we're done typechecking the function.
	c.symbols = originalTable
}

func (s *Statement) TypeCheck(c *Compiler) {
	if s.Decl != nil {
		s.Decl.TypeCheck(c)
	} else if s.Labeled != nil {
		s.Labeled.TypeCheck(c)
	} else if s.Simple != nil {
		s.Simple.TypeCheck(c)
	} else if s.Return != nil {
		s.Return.TypeCheck(c)
	} else if s.Continue != nil {
		s.Continue.TypeCheck(c)
	} else if s.Goto != nil {
		s.Goto.TypeCheck(c)
		//} else if s.Fallthrough != nil {
		//	s.Fallthrough.TypeCheck(c)
	} else if s.If != nil {
		s.If.TypeCheck(c)
	} else if s.For != nil {
		s.For.TypeCheck(c)
	} else { // It's a Block
		prevST := c.symbols
		c.symbols = newSymbolTable(c.symbols)
		for _, stmt := range s.Block {
			stmt.TypeCheck(c)
		}
		c.symbols = prevST
	}
}

func (d *Declaration) TypeCheck(c *Compiler) {
	if d.Var != nil {
		d.Var.TypeCheck(c)
	} else if d.Short != nil {
		d.Short.TypeCheck(c)
	}
}

func (v *VarDecl) TypeCheck(c *Compiler) {
	// Add these symbols to the current symbol table.
	for _, name := range v.Names {
		c.symbols.add(name, v.Type)
	}

	if len(v.Exprs) > 0 {
		if len(v.Exprs) != len(v.Names) {
			log.Fatalf("Mismatch: declared %d variables, but gave %d expressions", len(v.Names), len(v.Exprs))
		}

		for _, expr := range v.Exprs {
			t := expr.TypeOf(c)
			if !c.assignable(t, v.Type) {
				log.Fatalf("Expected values of type %s but found %s", v.Type.String(), t.String())
			}
		}
	}
}

func (v *ShortVarDecl) TypeCheck(c *Compiler) {
	if len(v.Exprs) != len(v.Names) {
		log.Fatalf("Mismatched := sides: %d and %d", len(v.Names), len(v.Exprs))
	}

	for i, expr := range v.Exprs {
		c.symbols.add(v.Names[i], expr.TypeOf(c))
	}
}

func (s *LabeledStmt) TypeCheck(c *Compiler) {
	// TODO Actually handle labels.
	s.Stmt.TypeCheck(c)
}

func (s *SimpleStmt) TypeCheck(c *Compiler) {
	if s.Expr != nil {
		s.Expr.TypeOf(c) // TODO: Only certain kinds of expressions allowed? Usually calls, but not necessarily.
	} else if s.IncDec != nil {
		t := s.IncDec.Expr.TypeOf(c)
		if !(t == typeInt || t == typeUint) {
			log.Fatalf("Cannot %s an expression of type %s", s.IncDec.Op, t.String())
		}
	} else if s.Assign != nil {
		if len(s.Assign.Lhs) != len(s.Assign.Rhs) {
			log.Fatalf("Assignment operator has %d destinations on the left, and %d values on the right", len(s.Assign.Lhs), len(s.Assign.Rhs))
		}

		if s.Assign.Op != "=" && len(s.Assign.Lhs) > 1 {
			log.Fatalf("Multi-value assignment cannot be used with %s", s.Assign.Op)
		}

		for i, expr := range s.Assign.Lhs {
			lhsType := expr.TypeOf(c)
			rhsType := s.Assign.Rhs[i].TypeOf(c)
			if !c.convertible(rhsType, lhsType) {
				log.Fatalf("Cannot convert %s into %s", lhsType.String(), rhsType.String())
			}
		}
	} else if s.ShortDecl != nil {
		for i, expr := range s.ShortDecl.Exprs {
			t := expr.TypeOf(c)
			c.symbols.add(s.ShortDecl.Names[i], t)
		}
	} else {
		log.Fatal("Internal error: Illegal statement - no alternative set")
	}
}

func (s *ReturnStmt) TypeCheck(c *Compiler) {
	// If this function has a return type, check that we're supplying a value
	// assignable to it.
	funcType := c.symbols.lookup(c.this)
	if funcType.Function.ReturnType == nil {
		if s.Expr != nil {
			log.Fatalf("Tried to return a value from %s, but it has no return type", c.this)
		}
	} else {
		t := s.Expr.TypeOf(c)
		if !c.assignable(t, funcType.Function.ReturnType) {
			log.Fatalf("Cannot return %s from function with return type %s",
				t.String(), funcType.Function.ReturnType.String())
		}
	}
}

func (s *ContinueStmt) TypeCheck(c *Compiler) {
	// Nothing to do.
}
func (s *GotoStmt) TypeCheck(c *Compiler) {
	// Nothing to do.
}

func (s *IfStmt) TypeCheck(c *Compiler) {
	// Prepare a new symbol table.
	origST := c.symbols
	c.symbols = newSymbolTable(c.symbols)
	if s.Initializer != nil {
		s.Initializer.TypeCheck(c)
	}
	condType := s.Condition.TypeOf(c)
	if condType != typeBool {
		log.Fatalf("If condition must be of type bool, but found %s", condType.String())
	}

	prevST := c.symbols
	c.symbols = newSymbolTable(c.symbols) // This one is just for the body.
	for _, stmt := range s.IfBody {
		// Since the symbols defined in the initializer are visible to both branches
		// of the if, we need another symbol table nested here.
		stmt.TypeCheck(c)
	}
	c.symbols = prevST

	if s.Else != nil {
		// This recursively handles else-ifs.
		if s.Else.If != nil {
			s.Else.If.TypeCheck(c)
		}

		// And this is for the final else. It's empty if there's no else block, or
		// nothing in the else block.
		prevST = c.symbols
		c.symbols = newSymbolTable(c.symbols)
		for _, stmt := range s.Else.Body {
			stmt.TypeCheck(c)
		}
	}
	c.symbols = origST
}

func (s *ForStmt) TypeCheck(c *Compiler) {
	// Introduce a new symbol table in case there are initializers.
	origST := c.symbols
	c.symbols = newSymbolTable(c.symbols)

	if s.ForClause != nil {
		fc := s.ForClause
		if fc.Initializer != nil {
			fc.Initializer.TypeCheck(c)
		}
		if fc.Condition != nil {
			t := fc.Condition.TypeOf(c)
			if t != typeBool {
				log.Fatalf("For loop condition must be bool; found %s", t.String())
			}
		}
		if fc.Increment != nil {
			fc.Increment.TypeCheck(c)
		}
	} else if s.Condition != nil {
		t := s.Condition.TypeOf(c)
		if t != typeBool {
			log.Fatalf("For loop condition must be bool; found %s", t.String())
		}
	}

	for _, stmt := range s.Body {
		stmt.TypeCheck(c)
	}
	c.symbols = origST
}

func (t1 *Type) equals(t2 *Type) bool {
	if t1 == t2 {
		return true
	}
	if t1.Name != nil && t2.Name != nil {
		return t1.Name.Name == t2.Name.Name
	} else if t1.Array != nil && t2.Array != nil {
		return t1.Array.Inner.equals(t2.Array.Inner)
	} else if t1.Pointer != nil && t2.Pointer != nil {
		return t1.Pointer.Inner.equals(t2.Pointer.Inner)
	} else if t1.Struct != nil && t2.Struct != nil {
		if len(t1.Struct.Fields) != len(t2.Struct.Fields) {
			return false
		}
		for i, f1 := range t1.Struct.Fields {
			if !f1.Type.equals(t2.Struct.Fields[i].Type) {
				return false
			}
		}
		return true
	}

	return t1 == t2 ||
		(t1.Name != nil && t1.Name.Name == "int" && t2 == typeInt) ||
		(t1 == typeInt && t2.Name != nil && t2.Name.Name == "int") ||
		(t1.Name != nil && t1.Name.Name == "uint" && t2 == typeUint) ||
		(t1 == typeUint && t2.Name != nil && t2.Name.Name == "uint") ||
		(t1.Name != nil && t1.Name.Name == "bool" && t2 == typeBool) ||
		(t1 == typeBool && t2.Name != nil && t2.Name.Name == "bool") ||
		(t1.Name != nil && t1.Name.Name == "char" && t2 == typeChar) ||
		(t1 == typeChar && t2.Name != nil && t2.Name.Name == "char") ||
		(t1.Name != nil && t1.Name.Name == "string" && t2 == typeString) ||
		(t1 == typeString && t2.Name != nil && t2.Name.Name == "string")
}

/*
Returns true if a value of the first type can be assigned to a variable of the
second type. A value x of type V is assignable to a type T in any of these cases:
- V is identical to T
- V and T have identical underlying types and at least one of V or T is not a named type.
- x is nil, and T is a pointer, function, slice, map, channel or interface type.
*/
func (c *Compiler) assignable(from, to *Type) bool {
	if from.equals(to) {
		return true
	}
	uFrom := from.underlyingType(c)
	uTo := to.underlyingType(c)
	if uFrom.equals(uTo) {
		// Underlying types match, so they're fine unless both are named.
		return from.Name == nil || to.Name == nil
	}

	// Otherwise it's just the basics.
	return (uFrom == typeInt && uTo == typeUint) ||
		(uFrom == typeUint && uTo == typeInt) ||
		(uFrom == typeNil && uTo.Pointer != nil) ||
		(uFrom == typeString && uTo.Array != nil && uTo.Array.Inner == typeChar) ||
		(uFrom.Array != nil && uFrom.Array.Inner == typeChar && uTo == typeString)
}

func (t *Type) underlyingType(c *Compiler) *Type {
	if t == typeString {
		return &Type{Array: &ArrayType{Inner: typeChar}}
	} else if t.Name != nil {
		if t, ok := c.types[t.Name.Name]; ok {
			return t
		} else {
			log.Fatalf("Cannot resolve named type: %s", t.Name.Name)
		}
	} else if t.Pointer != nil {
		return &Type{Pointer: &PointerType{Inner: t.Pointer.Inner.underlyingType(c)}}
	} else if t.Array != nil {
		return &Type{Array: &ArrayType{Inner: t.Array.Inner.underlyingType(c)}}
	}
	return t
}

/*
A value x of type V is convertible to a type T in any of the following cases:
- V is assignable to T
- V and T have the same underlying type
- V and T are (unnamed) pointer types, and their pointer base types are convertible.
- V is []char and T is string
- V is string and T is []char

- ??? I might also include int<->ptr conversions. intptr and uintptr again?
*/
func (c *Compiler) convertible(from, to *Type) bool {
	if c.assignable(from, to) {
		return true
	}

	uFrom := from.underlyingType(c)
	uTo := to.underlyingType(c)
	if uFrom.equals(uTo) {
		return true // Same underlying type.
	}

	if uFrom.Pointer != nil && uTo.Pointer != nil {
		return c.convertible(uFrom.Pointer.Inner, uTo.Pointer.Inner)
	}
	if uFrom.Array != nil && uTo.Array != nil {
		return c.convertible(uFrom.Array.Inner, uFrom.Array.Inner)
	}
	return false
}

func (e *Expression) TypeOf(c *Compiler) *Type {
	return (*BinaryExpr)(e).TypeOf(c)
}

func (e *BinaryExpr) TypeOf(c *Compiler) *Type {
	if e.Left == nil {
		return e.Right.TypeOf(c)
	}

	// If Left is defined, this is an || operation.
	if lt := e.Left.TypeOf(c); lt != typeBool {
		log.Fatalf("Left-hand side of || must be bool, found %s", lt.String())
	}
	if rt := e.Right.TypeOf(c); rt != typeBool {
		log.Fatalf("Right-hand side of || must be bool, found %s", rt.String())
	}
	return typeBool
}

func (e *BinaryExpr1) TypeOf(c *Compiler) *Type {
	if e.Left == nil {
		return e.Right.TypeOf(c)
	}

	// If Left is defined, this is a && operation.
	if lt := e.Left.TypeOf(c); lt != typeBool {
		log.Fatalf("Left-hand side of && must be bool, found %s", lt.String())
	}
	if rt := e.Right.TypeOf(c); rt != typeBool {
		log.Fatalf("Right-hand side of && must be bool, found %s", rt.String())
	}
	return typeBool
}

// Exits with an error message if they don't match.
func requireMatchingIntegralTypes(lt, rt *Type, op string) {
	if (lt == typeInt && rt == typeInt) || (lt == typeUint && rt == typeUint) {
		return
	}
	log.Fatalf("Type mismatch in %s operation: %s does not match %s", op, lt.String(), rt.String())
}

// == != < <= > >= - matching sides and returns bool.
func (e *BinaryExpr2) TypeOf(c *Compiler) *Type {
	if e.Left == nil {
		return e.Right.TypeOf(c)
	}

	lt := e.Left.TypeOf(c)
	rt := e.Right.TypeOf(c)
	// == and != work on anything; others require int/uint.
	if e.Op == "==" || e.Op == "!=" {
		if !lt.equals(rt) {
			log.Fatalf("Type mismatch in %s operation: %s does not match %s", e.Op,
				lt.String(), rt.String())
		}
		return typeBool
	}

	requireMatchingIntegralTypes(lt, rt, e.Op)
	return typeBool
}

// + - | ^ - matching integer types
func (e *BinaryExpr3) TypeOf(c *Compiler) *Type {
	if e.Left == nil {
		return e.Right.TypeOf(c)
	}

	lt := e.Left.TypeOf(c)
	rt := e.Right.TypeOf(c)
	requireMatchingIntegralTypes(lt, rt, e.Op)
	return lt
}

// * / % & &^   matching integer types
// << >>        int/uint << uint, returns LHS
func (e *BinaryExpr4) TypeOf(c *Compiler) *Type {
	if e.Left == nil {
		return e.Right.TypeOf(c)
	}

	lt := e.Left.TypeOf(c)
	rt := e.Right.TypeOf(c)
	if e.Op == "<<" || e.Op == ">>" {
		if lt != typeInt && lt != typeUint {
			log.Fatalf("Left-hand operand of %s must be int or uint; found %s", e.Op, lt.String())
		}
		if rt != typeUint {
			log.Fatalf("Right-hand operand of %s must be uint; found %s", e.Op, rt.String())
		}
		return lt
	}

	// Otherwise, matching int types.
	requireMatchingIntegralTypes(lt, rt, e.Op)
	return lt
}

// Unary + - ! ^ * & and <-
func (e *UnaryExpr) TypeOf(c *Compiler) *Type {
	t := e.Expr.TypeOf(c)
	switch e.Op {
	case "":
		return t

	case "+", "-", "^":
		if t != typeInt && t != typeUint {
			log.Fatalf("Unary %s cannot be applied to %s", e.Op, t.String())
		}
		return t

	case "!":
		if t != typeBool {
			log.Fatalf("Unary ! cannot be applied to %s", t.String())
		}
		return typeBool

	case "*":
		if t.Pointer == nil {
			log.Fatalf("Cannot apply * to non-pointer type %s", t.String())
		}
		return t.Pointer.Inner

	case "&":
		return &Type{Pointer: &PointerType{Inner: t}}

	case "<-":
		log.Fatalf("Channels are not supported in go10c")
	}
	return nil
}

// A PrimaryExpr is EITHER:
// - A SimpleOperand, Conversion call, or builtin call; OR
// - An expression, with optionally a selector, index or call attached.
func (e *PrimaryExpr) TypeOf(c *Compiler) *Type {
	if e.Operand != nil {
		return e.Operand.TypeOf(c)
	}
	if e.Conversion != nil {
		return e.Conversion.TypeOf(c)
	}
	if e.Builtin != nil {
		return e.Builtin.TypeOf(c)
	}

	// Resolve the inner expression first.
	et := e.Expr.TypeOf(c)
	if e.Selector != "" {
		// Expect et to be a struct or pointer to a struct.
		etu := et.underlyingType(c)
		var structType *StructType
		if etu.Struct != nil {
			structType = etu.Struct
		} else if etu.Pointer != nil && etu.Pointer.Inner.Struct != nil {
			structType = etu.Pointer.Inner.Struct
		} else {
			log.Fatalf("Cannot access field '%s' on non-struct type %s", e.Selector, et.String())
		}

		for _, field := range structType.Fields {
			for _, name := range field.Names {
				if name == e.Selector {
					return field.Type
				}
			}
		}
		log.Fatalf("Unknown struct field '%s' on %s", e.Selector, et.String())
		return nil
	}

	if e.Index != nil {
		// Expect e to be an array type.
		if et.Array == nil {
			log.Fatalf("Cannot index into non-array type %s", et.String())
		}
		return et.Array.Inner
	}

	if e.Call != nil {
		// et must be a function type, and the provided arguments must be assignable
		// to them. Our type here is the return type, or typeVoid.
		ft := et.Function
		if ft == nil {
			log.Fatalf("Cannot call non-function type %s", et.String())
		}

		if len(ft.ParamTypes) != len(e.Call.Exprs) {
			log.Fatalf("Function call expected %d arguments but got %d", len(ft.ParamTypes), len(e.Call.Exprs))
		}

		for i, arg := range e.Call.Exprs {
			argType := arg.TypeOf(c)
			if !c.assignable(argType, ft.ParamTypes[i]) {
				log.Fatalf("Function call expected argument %d to be %s, but got %s",
					i+1, ft.ParamTypes[i].String(), argType.String())
			}
		}

		if ft.ReturnType == nil {
			return typeNil
		}
		return ft.ReturnType
	}

	// If we're still here, something went wrong.
	log.Fatalf("Can't happen: bottom of PrimaryExpr.TypeOf.")
	return typeNil
}

func (e *SimpleOperand) TypeOf(c *Compiler) *Type {
	if e.Lit != nil {
		return e.Lit.TypeOf(c)
	} else if e.SubExpr != nil {
		return e.SubExpr.TypeOf(c)
	} else if e.Var != nil {
		return c.symbols.lookup(e.Var.Name)
	}
	log.Fatalf("Can't happen: bottom of SimpleOperand.TypeOf")
	return typeNil
}

func (e *Literal) TypeOf(c *Compiler) *Type {
	if e.Basic != nil {
		return e.Basic.TypeOf(c)
	}

	// Otherwise it's a type{...} combo.
	if e.LitType.Struct != nil { // Provided a literal struct{...} type.
		typeCheckStruct(c, e.LitType.Struct, e.LitVal)
		// If that didn't error out, we're good.
		return &Type{Struct: e.LitType.Struct}
	} else if e.LitType.Array != nil { // An array literal.
		typeCheckArray(c, e.LitType.Array, e.LitVal)
		// If that succeed, it's the array type.
		return &Type{Array: e.LitType.Array}
	} else {
		typeName := *e.LitType.Name
		if t, ok := c.types[typeName]; ok {
			if t.Array != nil {
				typeCheckArray(c, t.Array, e.LitVal)
			} else if t.Struct != nil {
				typeCheckStruct(c, t.Struct, e.LitVal)
			} else {
				log.Fatalf("Cannot create inline literal of type which is not array or struct: %s", t.String())
			}
		} else {
			log.Fatalf("Unknown type name '%s'", typeName)
		}
	}
	log.Fatalf("Can't happen: bottom of Literal.TypeOf")
	return typeNil
}

func typeCheckStruct(c *Compiler, st *StructType, elements []*Element) {
	// Expect keyword style, not necessarily in order.
	fieldTypes := map[string]*Type{}
	provided := map[string]bool{}
	for _, field := range st.Fields {
		for _, name := range field.Names {
			fieldTypes[name] = field.Type
			provided[name] = false
		}
	}

	for _, elem := range elements {
		if elem.KeyName == nil {
			log.Fatalf("Key: value pairs required for struct literals")
		}
		if expectedType, ok := fieldTypes[*(elem.KeyName)]; ok {
			// Check that the provided value has the right type.
			givenType := elem.KeyExpr.TypeOf(c)
			if !c.assignable(givenType, expectedType) {
				log.Fatalf("Field %s expected %s but got %s", *elem.KeyName, expectedType.String(), givenType.String())
			}
			provided[*elem.KeyName] = true
		} else {
			log.Fatalf("Struct does not have a key '%s'", *elem.KeyName)
		}
	}

	for name, p := range provided {
		if !p {
			log.Fatalf("Struct literals must be fully populated - no value provided for '%s'", name)
		}
	}
	// If we're still here, we've successfully defined the literal.
}

func typeCheckArray(c *Compiler, at *ArrayType, elements []*Element) {
	// Expects non-keyword arguments, in the order of the array.
	for _, elem := range elements {
		if elem.Value != nil {
			t := elem.Value.TypeOf(c)
			if !c.assignable(t, at.Inner) {
				log.Fatalf("Found a value of type %s in %s array literal", t.String(), at.String())
			}
		} else {
			log.Fatalf("Array literals cannot contain key-value pairs")
		}
	}
}

func (e *BasicLit) TypeOf(c *Compiler) *Type {
	if e.Int != nil {
		return typeInt
	} else if e.Bool != nil {
		return typeBool
	} else if e.Char != nil {
		return typeChar
	} else if e.String != nil {
		return typeString
	}
	log.Fatalf("Can't happen: BasicLit with no pointers set")
	return typeNil
}

func (e *Conversion) TypeOf(c *Compiler) *Type {
	// Conversion is allowed between:
	// - arrays of assignable types
	// - structs with the same underlying types
	// - functions with assignable arguments and return types
	// - named types whose underlying representations are convertible.

	// Convert the NonPointerType into a *Type, for compatibility.
	trueType := &Type{Array: e.Type.Array, Struct: e.Type.Struct, Function: e.Type.Func, Name: e.Type.Name}
	if e.Type.Wrapped != nil {
		trueType = e.Type.Wrapped
	}

	found := e.Expr.TypeOf(c)
	if c.convertible(found, trueType) {
		return trueType
	}
	log.Fatalf("Cannot convert %s to %s", found.String(), trueType.String())
	return typeNil
}

func (e *BuiltinCall) TypeOf(c *Compiler) *Type {
	// new expects a type only.
	if e.Name == "new" {
		if e.Type == nil {
			log.Fatalf("new() expects a type name; none was provided")
		}
		if len(e.Exprs) > 0 {
			log.Fatalf("new() cannot accept expressions")
		}
		return &Type{Pointer: &PointerType{Inner: e.Type}}
	} else if e.Name == "delete" {
		// Expects no type, and a set of things to delete.
		if e.Type != nil {
			log.Fatalf("No type can be supplied to delete()")
		}
		return typeVoid
	} else if e.Name == "panic" {
		// Panic accepts any single value, returns void.
		if e.Type != nil {
			log.Fatalf("No type can be supplied to panic()")
		}
		if len(e.Exprs) > 1 {
			log.Fatalf("panic() expects 0 or 1 argument")
		}
		return typeVoid
	}
	log.Fatalf("Can't happen: bottom of BuiltinCall.TypeOf")
	return typeVoid
}
