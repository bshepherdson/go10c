package main

import (
	"go/token"
	"log"

	"github.com/davecgh/go-spew/spew"
)

func (v *VarDecl) TypeCheck(c *Compiler) {
	if v.Value != nil {
		t := v.Value.TypeOf(c)
		if !t.AssignableTo(v.Type) {
			c.typeError(v, "Variable '%s' declared to have type %s, but right-hand side is %s", v.Name, v.Type.String(), t.String())
		}
	}
}

func (t *TypeDecl) TypeCheck(c *Compiler) {
	// Nothing to check here.
}

func (f *FuncDecl) TypeCheck(c *Compiler) {
	origST := c.symbols
	c.symbols = newSymbolTable(c.symbols)
	c.this = f.Name

	for _, v := range f.Args {
		for _, name := range v.Names {
			c.symbols.add(name, v.Type)
		}
	}
	if f.Receiver != nil {
		c.symbols.add(f.Receiver.Names[0], f.Receiver.Type)
	}

	for _, stmt := range f.Body {
		stmt.TypeCheck(c)
	}

	c.symbols = origST
}

// Statements
func (a *AssignStmt) TypeCheck(c *Compiler) {
	if len(a.Lhs) != len(a.Rhs) {
		c.typeError(a, "Wrong number of values in assignment: %d on left, %d on right", len(a.Lhs), len(a.Rhs))
		return
	}

	if a.Op == token.DEFINE {
		// Introduce a new symbol table, and add the defined symbols to it.
		c.symbols = newSymbolTable(c.symbols)
		for i, expr := range a.Rhs {
			t := expr.TypeOf(c)
			if v, ok := a.Lhs[i].(*Ident); ok {
				c.symbols.add(v.Name, t)
			} else {
				c.typeError(a, "Found non-identifier on left-hand side of :=")
			}
		}
	} else if a.Op == token.ASSIGN {
		// RHS must be assignable to LHS.
		for i, expr := range a.Rhs {
			provided := expr.TypeOf(c)
			expected := a.Lhs[i].TypeOf(c)
			if !a.Lhs[i].Lvalue() {
				c.typeError(a.Lhs[i], "Left-hand side of assignment is not writable")
			}
			if !provided.AssignableTo(expected) {
				c.typeError(expr, "%s cannot be assigned to %s", provided.String(), expected.String())
			}
		}
	} else if a.Op == token.SHL_ASSIGN || a.Op == token.SHR_ASSIGN {
		// LHS must be uint or int, RHS must be uint.
		for i, rhs := range a.Rhs {
			lhs := a.Lhs[i]
			lt := lhs.TypeOf(c)
			rt := rhs.TypeOf(c)
			if !lt.Underlying().Equals(typeInt) && !lt.Underlying().Equals(typeUint) {
				c.typeError(lhs, "Left-hand side of <<= or >>= must be int or uint, found %s", lt.String())
			} else if !rt.Underlying().Equals(typeUint) {
				c.typeError(rhs, "Right-hand side of <<= or >>= must be uint, found %s", rt.String())
			} else if lhs.Lvalue() {
				c.typeError(lhs, "Left-hand side of assignment must be writable")
			}
			// Allowed.
		}
	} else { // Other adjustments: matching int types, lvalues.
		for i, rhs := range a.Rhs {
			lhs := a.Lhs[i]
			lt := lhs.TypeOf(c)
			rt := rhs.TypeOf(c)
			ltu := lt.Underlying()
			rtu := rt.Underlying()
			if !ltu.Equals(typeInt) && !ltu.Equals(typeUint) {
				c.typeError(lhs, "Left-hand side of %s must be int or uint, found %s", a.Op.String(), lt.String())
			} else if !rtu.Equals(typeInt) && !rtu.Equals(typeUint) {
				c.typeError(rhs, "Right-hand side of %s must be int or uint, found %s", a.Op.String(), rt.String())
			} else if !ltu.Equals(rtu) {
				c.typeError(lhs, "Left and right sides of %s must match; found %s and %s", a.Op.String(), lt.String(), rt.String())
			}
			// Otherwise, it's allowed.
		}
	}
}

func (e *ExprStmt) TypeCheck(c *Compiler) {
	// Make sure the types are sound, but we don't need to do anything with it.
	e.Expr.TypeOf(c)
}

func (r *ReturnStmt) TypeCheck(c *Compiler) {
	f, found := c.symbols.lookup(c.this)
	if !found {
		log.Fatalf("Can't happen: Function %s cannot be found", c.this)
	}
	ft, ok := f.Underlying().(*FuncType)
	if !ok {
		panic("Can't happen: Function is not a FuncType")
	}

	if len(r.Results) == 0 && ft.ReturnType == nil {
		// No return values, so we're good.
		return
	}

	if ft.ReturnType == nil && len(r.Results) > 0 {
		c.typeError(r, "Cannot return a value from a void function")
	}

	if len(r.Results) > 1 {
		c.typeError(r, "Multiple return values are not supported")
	}
	res := r.Results[0]
	t := res.TypeOf(c)
	if !t.AssignableTo(ft.ReturnType) {
		c.typeError(r, "Returned value has type %s which cannot be assigned to %s", t.String(), ft.ReturnType.String())
	}
}

// Expressions
func (x *Ident) TypeOf(c *Compiler) Type {
	spew.Dump("Ident", x)
	if x.Name == "false" || x.Name == "true" {
		return typeBool
	}

	if t, ok := c.symbols.lookup(x.Name); ok {
		return t
	}
	c.typeError(x, "Unknown identifier '%s'", x.Name)
	return nil
}

func (x *NumLit) TypeOf(c *Compiler) Type {
	return typeInt
}

func (x *CharLit) TypeOf(c *Compiler) Type {
	return typeChar
}

func (x *StringLit) TypeOf(c *Compiler) Type {
	return typeString
}

// Legal operations: + - ! ^ * &, plus unsupported <-
func (x *UnaryExpr) TypeOf(c *Compiler) Type {
	t := x.Expr.TypeOf(c)
	tu := t.Underlying()
	switch x.Op {
	case token.ADD, token.SUB, token.XOR:
		// Integers in and out.
		if tu.Equals(typeInt) || tu.Equals(typeUint) {
			return t
		}
		c.typeError(x, "Cannot apply unary %s to %s", x.Op.String(), t.String())

	case token.NOT:
		if tu.Equals(typeBool) {
			return t
		}
		c.typeError(x, "Cannot apply ! to %s", t.String())

	case token.AND:
		return &PointerType{Inner: t}

	case token.MUL:
		if pt, ok := tu.(*PointerType); ok {
			return pt.Inner
		}
		c.typeError(x, "Cannot dereference non-pointer type %s", t.String())
	}
	c.typeError(x, "Unknown unary operator %s", x.Op.String())
	return nil
}

// Legal operations, split by types
// ADD SUB MUL QUO REM AND OR XOR AND_NOT - matching integer types, return same
// SHL SHR - either int on left, uint on right, leturn LHS
// LAND LOR - bool x bool -> bool
// LSS GTR LEQ GEQ - matching ints, return bool
// EQL NEQ  - anything matching, return bool
func (b *BinaryExpr) TypeOf(c *Compiler) Type {
	lt := b.Lhs.TypeOf(c)
	rt := b.Rhs.TypeOf(c)
	ltu := lt.Underlying()
	rtu := rt.Underlying()

	switch b.Op {
	// Matching integer types, return same.
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM, token.AND, token.OR, token.XOR, token.AND_NOT:
		if matchingInts(ltu, rtu) {
			return lt
		}
		c.typeError(b, "Type mismatch: %s %s %s", lt.String(), b.Op.String(), rt.String())

	case token.SHL, token.SHR:
		if ltu.Equals(typeInt) || ltu.Equals(typeUint) {
			if rtu.Equals(typeUint) {
				return lt
			}
			c.typeError(b, "Right-hand side of %s must be uint, found %s", b.Op.String(), rt.String())
		} else {
			c.typeError(b, "Left-hand side of %s must be int or uint, found %s", b.Op.String(), lt.String())
		}

	case token.LAND, token.LOR:
		if ltu.Equals(typeBool) && rtu.Equals(typeBool) {
			return typeBool
		}
		c.typeError(b, "Type mismatch in %s: required bool, got %s and %s", b.Op.String(), lt.String(), rt.String())

	case token.LSS, token.GTR, token.LEQ, token.GEQ:
		if matchingInts(ltu, rtu) {
			return typeBool
		}
		c.typeError(b, "Type mismatch in %s: expected int, uint or char, got %s and %s", b.Op.String(), lt.String(), rt.String())

	case token.EQL, token.NEQ:
		// Anything matching, returns bool.
		if ltu.Equals(rtu) {
			return typeBool
		}
		c.typeError(b, "Type mismatch in %s: types must match, but got %s and %s", lt.String(), rt.String())
	}

	c.typeError(b, "Unknown binary operator: %s", b.Op.String())
	return nil
}

func (e *CallExpr) TypeOf(c *Compiler) Type {
	// Check the arguments against the function's type.
	ft, ok := e.Fun.TypeOf(c).(*FuncType)
	if !ok {
		c.typeError(e, "Cannot call non-function type %s", e.Fun.TypeOf(c))
	}

	if len(ft.Args) != len(e.Args) {
		c.typeError(e, "Function wants %d arguments but got %d", len(ft.Args), len(e.Args))
	}

	for i, a := range e.Args {
		expected := ft.Args[i]
		found := a.TypeOf(c)
		if !found.AssignableTo(expected) {
			c.typeError(e, "Argument %d expected %s but found %s", i+1, expected.String(), found.String())
		}
	}

	return ft.ReturnType // Nil if no return value, which serves to signal void.
}

// Expr must be a map or an array. Since maps are not supported, it's just
// arrays.
func (e *IndexExpr) TypeOf(c *Compiler) Type {
	t := e.Expr.TypeOf(c)
	tu := t.Underlying()
	it := e.Index.TypeOf(c)
	itu := it.Underlying()
	if at, ok := tu.(*ArrayType); ok {
		if itu.Equals(typeInt) || itu.Equals(typeUint) {
			return at.Inner
		}
		c.typeError(e.Index, "Index type must be int or uint, but found %s", it.String())
	}
	c.typeError(e, "Cannot index non-array type %s", t.String())
	return nil
}

// Expr must be a struct, so we can access fields on it.
func (s *SelectorExpr) TypeOf(c *Compiler) Type {
	t := s.Expr.TypeOf(c)
	tu := t.Underlying()
	st, ok := tu.(*StructType)
	if !ok {
		if pt, ok_ := tu.(*PointerType); ok_ {
			st, ok = pt.Inner.Underlying().(*StructType)
		}
	}

	if ok {
		// Check for a field with that name.
		for _, field := range st.Fields {
			for _, name := range field.Names {
				if name == s.Selector {
					return field.Type
				}
			}
		}
		c.typeError(s, "%s has no field '%s'", t.String(), s.Selector)
	}
	c.typeError(s, "Cannot access field of non-struct type %s", t.String())
	return nil
}

func matchingInts(lt, rt Type) bool {
	return (lt.Equals(typeInt) && rt.Equals(typeInt)) ||
		(lt.Equals(typeUint) && rt.Equals(typeUint)) ||
		(lt.Equals(typeChar) && rt.Equals(typeChar))
}

func (c *Compiler) typeCheckAll(pkg *Package) {
	for _, d := range pkg.Decls {
		d.TypeCheck(c)
		spew.Dump(d)
	}
}
