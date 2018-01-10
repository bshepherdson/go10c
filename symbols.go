package main

import "log"

var typeInt BuiltinType = "int"
var typeUint BuiltinType = "uint"
var typeBool BuiltinType = "bool"
var typeChar BuiltinType = "char"
var typeString BuiltinType = "string"

type SymbolTable struct {
	parent  *SymbolTable
	symbols map[string]Type
}

func newSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{parent, map[string]Type{}}
}

// Returns nil if no symbol with that name is found.
func (st *SymbolTable) lookup(sym string) Type {
	if t, ok := st.symbols[sym]; ok {
		return t
	}
	if st.parent == nil {
		return nil
	}
	return st.parent.lookup(sym)
}

func (st *SymbolTable) add(name string, t Type) {
	if _, ok := st.symbols[name]; ok {
		log.Fatalf("duplicate symbol: %s", name)
	}
	st.symbols[name] = t
}

func (f *FuncDecl) CollectSymbols(st *SymbolTable) {
	ft := &FuncType{ReturnType: f.ReturnType}
	for _, argGroup := range f.Args {
		for _, _ = range argGroup.Names {
			ft.Args = append(ft.Args, argGroup.Type)
		}
	}
	st.add(f.Name, ft)
}

func (v *VarDecl) CollectSymbols(st *SymbolTable) {
	st.add(v.Name, v.Type)
}

func (t *TypeDecl) CollectSymbols(st *SymbolTable) {
	// Nothing to collect for types, those go separately.
}

func collectSymbols(pkg *Package) (*SymbolTable, map[string]Type) {
	syms := newSymbolTable(nil)
	types := map[string]Type{}
	for _, di := range pkg.Decls {
		if td, ok := di.(*TypeDecl); ok {
			if _, ok := types[td.Name]; ok || td.Name == "int" || td.Name == "uint" || td.Name == "string" || td.Name == "bool" {
				log.Fatalf("duplicate type declaration: %s", td.Name)
			} else {
				types[td.Name] = td.Type
			}
		} else {
			di.CollectSymbols(syms)
		}
	}
	return syms, types
}
