package main

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
	// TODO: Check the body.
}

func (c *Compiler) typeCheckAll(pkg *Package) {
	// TODO
}
