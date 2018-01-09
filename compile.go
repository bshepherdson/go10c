package main

import (
	"fmt"
	"log"
	"os"
	"path"
	"strings"
)

// Compiled output notes:
// I and J are useful because of STI and STD for tighter loops.
// So Z is used as the frame pointer.

// Calling convention:
// Arguments in A, B, C.
// Return values in A, B and C.
// Stack diagram inside a function.
// old values...
// arg 6
// arg 5
// arg 4
// old PC
// old FP         <-- FP (Z)
// local N
// ...
// local 0
// saved value 2
// saved value 1  <-- SP

type SymbolTable struct {
	parent  *SymbolTable
	symbols map[string]*Type
}

type Compiler struct {
	packages     map[string]*Package
	symbols      *SymbolTable // The current context's symbols.
	regs         *RegisterState
	strings      map[string]string    // String literals to write into the binary.
	types        map[string]*Type     // Holds type aliases
	args         map[string]*Location // Argument names and their current locations.
	locals       map[string]*Location // Local variables and their current locations.
	globals      map[string]*Location // Global variables and their current locations.
	initializers []*Asm               // Code for the initializers of globals.
	code         []*Asm               // Main line code.
	this         string               // Name of the current function being compiled, used for labels.
	packageName  string               // Current package name.
	unique       int                  // Counter for unique labels.
}

type Location interface {
}

type LocReg string
type LocStack int    // Relative to the frame pointer (Z).
type LocLabel string // Stored at this label.
type LocConstant int // A constant value.

type Asm struct {
	op      string
	a       *Arg
	b       *Arg
	comment string // Useful for debugging.
}

var atReg = "reg"
var atPush = "push"
var atPop = "pop"
var atPeek = "peek"
var atSP = "sp"
var atPC = "pc"
var atEX = "ex"
var atLit = "lit"
var atAddrLit = "addrLit"
var atAddrReg = "addrReg"
var atAddrRegLit = "addrRegLit"
var atAddrLabel = "addrLabel"
var atLabel = "label"

type Arg struct {
	argType  string
	intValue int
	strValue string
}

type ExprResult struct {
	code   []*Asm
	result *Arg
}

type RegisterState struct {
}

func newRegisterState() *RegisterState {
	return new(RegisterState)
}

type Outputtable interface {
	Output() string
}

func (a *Asm) Output() string {
	if a.comment != "" {
		return "; " + a.comment
	}

	b := ""
	if a.b != nil {
		b = a.b.Output() + ", "
	}
	return a.op + b + a.a.Output()
}

func (a *Arg) Output() string {
	switch a.argType {
	case atReg:
		return a.strValue // Register name
	case atPush:
		return "push"
	case atPop:
		return "pop"
	case atPeek:
		return "peek"
	case atSP:
		return "sp"
	case atPC:
		return "pc"
	case atEX:
		return "ex"
	case atLit:
		return fmt.Sprintf("%d", a.intValue)
	case atAddrLit:
		return fmt.Sprintf("[%d]", a.intValue)
	case atAddrRegLit:
		return fmt.Sprintf("[%s + %d]", a.strValue, a.intValue)
	case atAddrLit:
		return fmt.Sprintf("[%s]", a.strValue)
	case atLabel:
		return a.strValue
	default:
		log.Fatalf("Unknown arg type: %s", a.argType)
		return "" // Unreachable.
	}
}

// So here's the flow:
// - Parse all input files, recursively chasing imports, until the entire block
//   of code is loaded into memory.
// - Create a new, single Compiler instance.
// - Run over the entire block collecting global symbols and giving them labels.
// - Then we can compile all the top-level declarations in any order.

// Compiling a top-level variable:
// - Make memory for it.
// - If it's a constant, resolve it and set it in the assembly.
// - If it's not constant, add its code to the initializers.

// Register allocator: TODO
// TODO: For now, I'm going with dumb greedy allocation, and writing everything
// out all the time. Replace that with something much more optimal eventually.
// - Graph-based, trying to colour the graph such that registers are used
//   efficiently.
// - Static single assignment style: each value gets a unique tag, locations
//   (registers and stacked locations are used freely; global locations can't be
//   used for other things).
// - Values in temporary locations are always clean. Values in globals can be
//   "dirty" and need writing back.
// - Register states can have multiple checkpoints that need to be unified, such
//   as the branches of ifs, loops, switches and things.

// Compiling a function:
// - Set up a new scope, emit its label, and set up the initial locations.
// - Emit a prologue that saves FP and allocates space for the local slots.
// - Start evaluating each instruction.
// - Emit the epilogue.

func newCompiler() *Compiler {
	c := new(Compiler)
	c.packages = map[string]*Package{}
	c.symbols = newSymbolTable(nil)
	c.regs = newRegisterState()
	c.strings = map[string]string{}
	c.types = map[string]*Type{}
	c.globals = map[string]*Location{}
	c.initializers = make([]*Asm, 0, 256)
	c.code = make([]*Asm, 0, 4096)
	c.unique = 1
	return c
}

type Package struct {
	symbols *SymbolTable
	types   map[string]*Type
}

// Turns a block of parsed files into the output.
func Compile(asts []*GoFile, libraryPaths []string) []string {
	c := newCompiler()
	c.args = map[string]*Location{}

	// First pass: Build packages across their multiple files.
	packages := map[string]*GoFile{}
	for _, f := range asts {
		if p, ok := packages[f.Package]; ok {
			p.Imports = append(p.Imports, f.Imports...)
			p.Decls = append(p.Decls, f.Decls...)
		} else {
			packages[f.Package] = f
		}
	}

	// Second pass: For each package, recursively expand its imports.
	// We collect just the top-level packages first, then expand them, since it
	// isn't safe to modify the packages map while iterating over it.
	originals := make([]*GoFile, 0, len(packages))
	for _, p := range packages {
		originals = append(originals, p)
	}
	for _, p := range originals {
		importClosure(p, libraryPaths, packages)
	}

	// Third pass: Collecting all the global symbols.
	// Take their word for their types for now. Typechecking will catch the liars.
	for pkgName, pkg := range packages {
		c.packageName = pkgName
		baseName := strings.Split(pkgName, "/")
		c.packages[baseName[len(baseName)-1]] = c.collectSymbols(pkg)
	}

	// Fourth pass: Type checking all the symbols.
	for _, pkg := range packages {
		c.typeCheckAll(pkg)
	}

	fmt.Println("Successfully parsed and typechecked.")
	for st := c.symbols; st != nil; st = st.parent {
		for sym, t := range st.symbols {
			fmt.Printf("Symbol %s has type %s\n", sym, t.String())
		}
	}

	return []string{}
}

// Recursively includes all the packages imported by the given package.
func importClosure(pkg *GoFile, libraryPaths []string, packages map[string]*GoFile) {
	for _, i := range pkg.Imports {
		if _, ok := packages[i]; ok {
			continue // Found it already loaded.
		}

		// Otherwise, look for libraryPaths[x] + "/" + i to be a directory.
		importParts := strings.Split(i, "/") // Always /, not system separators.
		importParts = append([]string{""}, importParts...)
		for _, p := range libraryPaths {
			importParts[0] = p
			dirName := path.Join(importParts...)

			fi, err := os.Stat(dirName)
			if err != nil || fi == nil || !fi.IsDir() {
				continue
			}

			// Found our import! Parse all the *.go files and add them as a new
			// package.
			imp := &GoFile{Package: i}
			dir, err := os.Open(dirName)
			if err != nil {
				log.Fatalf("error reading imports from %s: %v", dirName, err)
			}

			contents, err := dir.Readdirnames(0)
			if err != nil {
				log.Fatalf("error scanning directory contents: %v", err)
			}
			for _, filename := range contents {
				if path.Ext(filename) == "go" {
					g, err := Parse(filename)
					if err != nil {
						log.Fatalf("parse error in imported library %s: %v", i, err)
					}
					imp.Imports = append(imp.Imports, g.Imports...)
					imp.Decls = append(imp.Decls, g.Decls...)
				}
			}

			packages[i] = imp
			importClosure(imp, libraryPaths, packages)
			break
		}
	}
}
