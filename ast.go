package main

import (
	"go/ast"
	"go/token"
	"log"
)

// Since we cannot declare methods on foreign types, we follow parsing with a
// processing pass where we use ast.Walk to comb the tree and convert everything
// into our local formats.

type AST interface {
	Loc() token.Pos
	CollectSymbols(*SymbolTable)
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

type Expr interface {
	TypeOf(*Compiler) Type
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
func astDecl(d ast.Decl) []Decl {
	gd, ok := d.(*ast.GenDecl)
	if !ok {
		panic("Y U NO GenDecl")
	}

	switch gd.Tok {
	case token.TYPE:
		decls := []Decl{}
		for _, spec := range gd.Specs {
			if ts, ok := spec.(*ast.TypeSpec); ok {
				// ts.Name, ts.Type
				td := &TypeDecl{Name: ts.Name.Name, Type: astType(ts.Type)}
				decls = append(decls, td)
			} else {
				log.Fatalf("Can't happen, token.TYPE but not TypeSpec")
			}
		}
		return decls
	case token.VAR:
		decls := []Decl{}
		for _, spec := range gd.Specs {
			if ts, ok := spec.(*ast.ValueSpec); ok {
				// ts.Names, ts.Type, ts.Values
				t := astType(ts.Type)
				for i, ident := range ts.Names {
					decls = append(decls, &VarDecl{Name: ident.Name, Type: t, Value: astExpr(ts.Values[i])})
				}
			} else {
				log.Fatalf("Can't happen, token.TYPE but not TypeSpec")
			}
		}
		return decls
	}
	log.Fatalf("Can't happen: bottom of astDecl")
	return []Decl{}
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
	}
	log.Fatalf("Bottom of astType")
	return nil
}

func astExpr(expr ast.Expr) Expr {
	return nil // TODO
}

func (a *Package) Loc() token.Pos  { return a.Pos }
func (a *VarDecl) Loc() token.Pos  { return a.Pos }
func (a *FuncDecl) Loc() token.Pos { return a.Pos }
func (a *TypeDecl) Loc() token.Pos { return a.Pos }
