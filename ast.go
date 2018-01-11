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

// Control flow statements: break, continue, goto and fallthrough.
type BranchStmt struct {
	Pos     token.Pos
	Flavour token.Token
	Label   string // Target label, if any.
}

type DeclStmt struct {
	Decl *VarDecl
}

// An expression on its own line; generally this is a call.
type ExprStmt struct {
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

func astPackage(f *ast.File) *Package {
	p := &Package{Pos: f.Package, Name: f.Name.Name}
	for _, d := range f.Decls {
		d2 := astDecl(d)
		if d2 != nil {
			p.Decls = append(p.Decls, d2...)
		}
	}
	return p
}

// ast.Decl is eg. a var (...) block, with an []Spec, where Spec is ImportSpec,
// ValueSpec, TypeSpec or ValueSpec.
// Or it might be an ast.FuncDecl.
func astDecl(di ast.Decl) []Decl {
	switch d := di.(type) {
	case *ast.GenDecl:
		switch d.Tok {
		case token.TYPE:
			decls := []Decl{}
			for _, spec := range d.Specs {
				if ts, ok := spec.(*ast.TypeSpec); ok {
					// ts.Name, ts.Type
					td := &TypeDecl{Pos: d.TokPos, Name: ts.Name.Name, Type: astType(ts.Type)}
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
					t := astType(ts.Type)
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
			f.Receiver = astField(field)
		}

		for _, field := range d.Type.Params.List {
			f.Args = append(f.Args, astField(field))
		}

		for _, stmt := range d.Body.List {
			f.Body = append(f.Body, astStmt(stmt))
		}

		if d.Type.Results != nil {
			if len(d.Type.Results.List) > 1 {
				log.Fatalf("Multiple return values are not supported")
			} else if len(d.Type.Results.List) == 1 {
				f.ReturnType = astType(d.Type.Results.List[0].Type)
			}
		}

		return []Decl{f}
	}

	log.Fatalf("Can't happen: bottom of astDecl")
	return []Decl{}
}

func astField(f *ast.Field) *VarSpec {
	v := &VarSpec{Type: astType(f.Type)}
	for _, name := range f.Names {
		v.Names = append(v.Names, name.Name)
	}
	return v
}

func astType(ti ast.Expr) Type {
	switch t := ti.(type) {
	case *ast.ArrayType:
		// We don't support lengths, so we can ignore that clause.
		return &ArrayType{Inner: astType(t.Elt)}

	case *ast.FuncType:
		if len(t.Results.List) > 1 || (len(t.Results.List) == 1 && len(t.Results.List[0].Names) > 1) {
			log.Fatalf("Multiple return values are not supported")
		}
		theType := &FuncType{}
		for _, f := range t.Params.List {
			t2 := astType(f.Type)
			for range f.Names {
				theType.Args = append(theType.Args, t2)
			}
		}

		if len(t.Results.List) == 1 {
			if t.Results.List[0].Names[0] != nil {
				log.Fatalf("Named return values are not supported")
			}
			theType.ReturnType = astType(t.Results.List[0].Type)
		}
		return theType

	case *ast.StructType:
		st := &StructType{}
		if t.Fields == nil || t.Fields.List == nil || len(t.Fields.List) == 0 {
			return st
		}

		for _, f := range t.Fields.List {
			vs := &VarSpec{Type: astType(f.Type)}
			for _, n := range f.Names {
				vs.Names = append(vs.Names, n.Name)
			}
			st.Fields = append(st.Fields, vs)
		}

		return st

	case *ast.Ident:
		if t.Name == "int" || t.Name == "uint" || t.Name == "char" || t.Name == "bool" || t.Name == "string" {
			return BuiltinType(t.Name)
		}
		return &NamedType{Name: t.Name}
	}

	log.Fatalf("Bottom of astType")
	return nil
}

func astStmt(iStmt ast.Stmt) Statement {
	switch stmt := iStmt.(type) {
	case *ast.AssignStmt:
		return astAssignStmt(stmt)

		/*
			case *ast.BlockStmt:
				b := &BlockStmt{Pos: stmt.Lbrace}
				for _, s := range b.List {
					b.Body = append(b.Body, astStmt(s))
				}
				return b

			case *ast.BranchStmt:
				b := &BranchStmt{Pos: stmt.TokPos, Flavour: stmt.Tok}
				if stmt.Label != nil {
					b.Label = stmt.Label.Name
				}
				return b

			case *ast.DeclStmt:
				d := astDecl(stmt.Decl)
				if vd, ok := d.(*VarDecl); ok {
					return &DeclStmt{vd}
				}
				log.Fatal("Cannot declare types or functions inside functions.")

			case *ast.ExprStmt:
				return &ExprStmt{Expr: astExpr(stmt.X)}

			case *ast.ForStmt:
				return astForStmt(stmt)

			case *ast.IfStmt:
				return astIfStmt(stmt)

			case *ast.IncDecStmt:
				return &IncDecStmt{Pos: stmt.TokPos, Op: stmt.Tok, Expr: astExpr(stmt.X)}

			case *ast.LabeledStmt:
				return &LabeledStmt{Pos: stmt.Colon, Label: stmt.Label.Name, Stmt: astStmt(stmt.Stmt)}
		*/

	case *ast.ReturnStmt:
		r := &ReturnStmt{Pos: stmt.Return}
		for _, expr := range stmt.Results {
			r.Results = append(r.Results, astExpr(expr))
		}
		return r
	}
	return nil
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

	case *ast.Ident:
		return astIdent(x)
	}
	return nil // TODO
}

func astIdent(expr *ast.Ident) *Ident {
	return &Ident{Pos: expr.NamePos, Name: expr.Name}
}

func (a *Package) Loc() token.Pos    { return a.Pos }
func (a *VarDecl) Loc() token.Pos    { return a.Pos }
func (a *FuncDecl) Loc() token.Pos   { return a.Pos }
func (a *TypeDecl) Loc() token.Pos   { return a.Pos }
func (a *AssignStmt) Loc() token.Pos { return a.Pos }
func (a *ReturnStmt) Loc() token.Pos { return a.Pos }

func (a *Ident) Loc() token.Pos     { return a.Pos }
func (a *NumLit) Loc() token.Pos    { return a.Pos }
func (a *CharLit) Loc() token.Pos   { return a.Pos }
func (a *StringLit) Loc() token.Pos { return a.Pos }
func (a *UnaryExpr) Loc() token.Pos { return a.Pos }

func (x *Ident) Lvalue() bool     { return true }
func (x *NumLit) Lvalue() bool    { return false }
func (x *CharLit) Lvalue() bool   { return false }
func (x *StringLit) Lvalue() bool { return false }
func (x *UnaryExpr) Lvalue() bool { return false }
