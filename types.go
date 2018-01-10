package main

import (
	"log"
	"strings"
)

// Types (not part of the AST)
type Type interface {
	AssignableTo(Type) bool
	ConvertibleTo(Type) bool
	Equals(Type) bool
	Underlying() Type
	String() string
}

type BuiltinType string

type FuncType struct {
	Receiver   Type
	Args       []Type
	ReturnType Type
}

type StructType struct {
	Fields []*VarSpec
}

func (t BuiltinType) AssignableTo(oi Type) bool {
	return t.Equals(oi)
}

func (t BuiltinType) ConvertibleTo(oi Type) bool {
	o, ok := oi.(BuiltinType)
	if !ok {
		return false
	}
	if o == t {
		return true
	}

	return (o == "int" && t == "uint") || (o == "uint" && t == "int")
}

func (t BuiltinType) Equals(oi Type) bool {
	o, ok := oi.(BuiltinType)
	return ok && o == t
}

func (t BuiltinType) Underlying() Type {
	return t
}

func (t BuiltinType) String() string {
	return string(t)
}

// Functions are assignable only if each type is assignable in the appropriate
// direction.
func (t *FuncType) AssignableTo(ot Type) bool {
	o, ok := ot.(*FuncType)
	if !ok {
		return false
	}

	if t.Receiver != nil && o.Receiver != nil {
		if !t.Receiver.AssignableTo(o.Receiver) {
			return false
		}
	}
	if t.Receiver != nil || o.Receiver != nil { // Either, but not both: fail
		return false
	}

	if len(t.Args) != len(o.Args) {
		return false
	}

	for i, a := range t.Args {
		if !a.AssignableTo(o.Args[i]) {
			return false
		}
	}

	if t.ReturnType == nil && o.ReturnType == nil {
		return true
	}
	if t.ReturnType != nil && o.ReturnType != nil {
		// NB: This is backwards: the other return type must be assignable to this
		// one.
		return o.ReturnType.AssignableTo(t.ReturnType)
	}
	return false
}

func (t *FuncType) ConvertibleTo(ot Type) bool {
	return t.AssignableTo(ot)
}

func (t *FuncType) Equals(ot Type) bool {
	o, ok := ot.(*FuncType)
	if !ok {
		return false
	}

	if t.ReturnType != nil && o.ReturnType != nil {
		if !t.ReturnType.Equals(o.ReturnType) {
			return false
		}
	} else if t.ReturnType == nil && o.ReturnType == nil {
		// Do nothing, all good.
	} else {
		return false // Mismatch.
	}

	if t.Receiver == nil && o.Receiver == nil {
		// All good.
	} else if t.Receiver != nil && o.Receiver != nil {
		if !t.Receiver.Equals(o.Receiver) {
			return false
		}
	} else {
		return false // Mismatch.
	}

	if len(t.Args) != len(o.Args) {
		return false
	}
	for i, a := range t.Args {
		if !a.Equals(o.Args[i]) {
			return false
		}
	}
	return true
}

func (t *FuncType) Underlying() Type {
	return t
}

func (t *FuncType) String() string {
	receiver := ""
	if t.Receiver != nil {
		receiver = "(" + t.Receiver.String() + ")"
	}
	args := make([]string, 0)
	for _, a := range t.Args {
		args = append(args, a.String())
	}

	ret := ""
	if t.ReturnType != nil {
		ret = t.ReturnType.String()
	}

	return "func" + receiver + "(" + strings.Join(args, ", ") + ")" + ret
}

func (t *StructType) structTypeMatch(ot Type, checker func(Type, Type) bool) bool {
	o, ok := ot.(*StructType)
	if !ok {
		return false
	}

	if len(t.Fields) != len(o.Fields) {
		return false
	}

	for i, field := range t.Fields {
		if len(field.Names) != len(o.Fields[i].Names) {
			return false
		}
		// Field names are part of the type.
		for j, name := range field.Names {
			if name != o.Fields[i].Names[j] {
				return false
			}
		}

		if !checker(field.Type, o.Fields[i].Type) {
			return false
		}
	}
	return true
}

func (t *StructType) AssignableTo(ot Type) bool {
	return t.structTypeMatch(ot, Type.AssignableTo)
}
func (t *StructType) ConvertibleTo(ot Type) bool {
	return t.structTypeMatch(ot, Type.ConvertibleTo)
}
func (t *StructType) Equals(ot Type) bool {
	return t.structTypeMatch(ot, Type.Equals)
}

func (t *StructType) Underlying() Type {
	return t
}

func (t *StructType) String() string {
	fields := []string{}
	for _, f := range t.Fields {
		fields = append(fields, strings.Join(f.Names, ", ")+" "+f.Type.String())
	}
	return "struct { " + strings.Join(fields, ", ") + " }"
}

type ArrayType struct {
	Inner Type
}

// Arrays are assignable if their inner, underlying types are assignable.
func (t *ArrayType) AssignableTo(ot Type) bool {
	o, ok := ot.(*ArrayType)
	return ok && t.Inner.AssignableTo(o.Inner)
}

func (t *ArrayType) ConvertibleTo(ot Type) bool {
	return t.AssignableTo(ot)
}

func (t *ArrayType) Equals(ot Type) bool {
	o, ok := ot.(*ArrayType)
	return ok && t.Inner.Equals(o.Inner)
}

func (t *ArrayType) Underlying() Type {
	return &ArrayType{Inner: t.Inner.Underlying()}
}

func (t *ArrayType) String() string {
	return "[]" + t.Inner.String()
}

type PointerType struct {
	Inner Type
}

// Pointers are assignable and convertible only if their inner types are equal.
func (t *PointerType) AssignableTo(ot Type) bool  { return t.Equals(ot) }
func (t *PointerType) ConvertibleTo(ot Type) bool { return t.Equals(ot) }
func (t *PointerType) Equals(ot Type) bool {
	o, ok := ot.(*PointerType)
	return ok && t.Inner.Equals(o.Inner)
}
func (t *PointerType) Underlying() Type { return t }
func (t *PointerType) String() string {
	return "*" + t.Inner.String()
}

type NamedType struct {
	Name     string
	Compiler *Compiler
}

// Named types are follow their underlying types.
func (t *NamedType) AssignableTo(ot Type) bool {
	return t.Underlying().AssignableTo(ot.Underlying())
}

func (t *NamedType) ConvertibleTo(ot Type) bool {
	return t.Underlying().ConvertibleTo(ot.Underlying())
}

func (t *NamedType) Equals(ot Type) bool {
	return t.Underlying().Equals(ot.Underlying())
}

func (t *NamedType) Underlying() Type {
	if u, ok := t.Compiler.types[t.Name]; ok {
		return u.Underlying() // Keep drilling down, in case this is nested.
	}
	log.Fatalf("Unknown named type %s", t.Name)
	return nil
}
