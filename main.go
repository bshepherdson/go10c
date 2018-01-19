package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
)

func main() {
	//outputFile := flag.String("o", "out.asm", "File name for the generated DCPU-16 assembly code")
	flag.Parse()

	inputFiles := flag.Args()
	fset := token.NewFileSet()
	code := map[string]*ast.File{}
	for _, f := range inputFiles {
		c, err := Parse(fset, f)
		if err != nil {
			fmt.Println(err)
			return
		}

		code[f] = c
	}

	output := Compile(code, fset, []string{})
	for _, s := range output {
		fmt.Println(s)
	}
}
