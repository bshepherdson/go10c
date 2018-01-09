package main

import "strings"

func (t *Type) String() string {
	if t.Function != nil {
		return t.Function.String()
	} else if t.Struct != nil {
		return t.Struct.String()
	} else if t.Array != nil {
		return t.Array.String()
	} else if t.Pointer != nil {
		return t.Pointer.String()
	} else if t.Name != nil {
		return t.Name.Name
	} else { // Builtin
		return t.Builtin
	}
}

func (t *FunctionType) String() string {
	args := make([]string, len(t.ParamTypes))
	for i, arg := range t.ParamTypes {
		args[i] = arg.String()
	}
	s := "func (" + strings.Join(args, ", ") + ")"
	if t.ReturnType == nil {
		return s
	}
	return s + " " + t.ReturnType.String()
}

func (t *StructType) String() string {
	fields := make([]string, len(t.Fields))
	for i, f := range t.Fields {
		fields[i] = strings.Join(f.Names, ", ") + " " + f.Type.String()
	}
	return "struct { " + strings.Join(fields, ", ") + " }"
}

func (t *ArrayType) String() string {
	return "[]" + t.Inner.String()
}

func (t *PointerType) String() string {
	return "*" + t.Inner.String()
}
