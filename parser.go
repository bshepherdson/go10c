package main

import (
	"go/ast"
	"go/parser"
	"go/token"
)

func Parse(fset *token.FileSet, filename string) (*ast.File, error) {
	node, err := parser.ParseFile(fset, filename, nil, 0)
	if err != nil {
		return nil, err
	}
	return node, nil
}
