package main

import "log"

var typeInt = &Type{Builtin: "int"}
var typeUint = &Type{Builtin: "uint"}
var typeBool = &Type{Builtin: "bool"}
var typeChar = &Type{Builtin: "char"}
var typeString = &Type{Builtin: "string"}
var typeNil = &Type{Builtin: "nil"} // Special - nil is a member of every pointer and array type.
var typeVoid = &Type{}              // Special - the return type of a void function.

func newSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{parent, map[string]*Type{}}
}

// Returns nil if no symbol with that name is found.
func (st *SymbolTable) lookup(sym string) *Type {
	if t, ok := st.symbols[sym]; ok {
		return t
	}
	if st.parent == nil {
		return nil
	}
	return st.parent.lookup(sym)
}

func (st *SymbolTable) add(name string, t *Type) {
	if _, ok := st.symbols[name]; ok {
		log.Fatalf("duplicate symbol: %s", name)
	}
	st.symbols[name] = t
}

type SymbolCollection interface {
	CollectSymbols(*SymbolTable)
}

func (f *FuncDecl) CollectSymbols(st *SymbolTable) {
	ft := &FunctionType{ReturnType: f.ReturnType}
	for _, argGroup := range f.Args {
		for _, _ = range argGroup.Args {
			ft.ParamTypes = append(ft.ParamTypes, argGroup.Type)
		}
	}
	st.add(f.Name, &Type{Function: ft})
}

func (c *ConstDecl) CollectSymbols(st *SymbolTable) {
	st.add(c.Name, typeInt)
}

func (c *NamesAndType) CollectSymbols(st *SymbolTable) {
	for _, name := range c.Args {
		st.add(name, c.Type)
	}
}

func (v *VarDecl) CollectSymbols(st *SymbolTable) {
	for _, name := range v.Names {
		st.add(name, v.Type)
	}
}

func (c *Compiler) collectSymbols(pkg *GoFile) *Package {
	syms := newSymbolTable(nil)
	types := map[string]*Type{}
	for _, d := range pkg.Decls {
		if d.Func != nil {
			d.Func.CollectSymbols(syms)
		} else {
			if d.Simple.Const != nil {
				d.Simple.Const.CollectSymbols(syms)
			} else if d.Simple.Type != nil {
				name := d.Simple.Type.Name
				if _, ok := types[name]; ok || name == "int" || name == "uint" || name == "string" || name == "bool" {
					log.Fatalf("duplicate type declaration: %s", name)
				} else {
					types[d.Simple.Type.Name] = d.Simple.Type.Type
				}
			} else if d.Simple.Var != nil {
				d.Simple.Var.CollectSymbols(syms)
			} else {
				log.Fatal("short declarations are not legal at the top level")
			}
		}
	}
	return &Package{syms, types}
}
