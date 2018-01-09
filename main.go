package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/davecgh/go-spew/spew"
)

func main() {
	outputFile := flag.String("o", "out.asm", "File name for the generated DCPU-16 assembly code")
	flag.Parse()

	inputFiles := flag.Args()
	code := make([]*GoFile, 0)
	for _, f := range inputFiles {
		c, err := Parse(f)
		if err != nil {
			fmt.Println(err)
			return
		}

		code = append(code, c)
	}

	Compile(code, []string{})

	f, err := os.Create(*outputFile)
	if err != nil {
		panic(err)
	}
	spew.Fdump(f, code)
}
