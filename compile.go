package main

import (
	"fmt"
	"go/ast"
	"go/token"
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

// One of these values will be non-nil.

type Compiler struct {
	// Global state
	fset       *token.FileSet
	packages   map[string]*Package
	namespaces map[string]*Namespace
	symbols    *SymbolTable      // The current context's symbols.
	types      map[string]Type   // Holds type aliases
	strings    map[string]string // String literals to write into the binary.

	// Code generation state
	cpu        CPU
	alloc      *Allocator
	data       map[string]*Data // Location stacks for the locals and arguments.
	globals    map[string]*Data // Location stacks for the globals.
	localCount int              // Running count of local slots in this function.

	initializers   []Outputtable // Code for the initializers of globals.
	code           []Outputtable // Main line code.
	this           string        // Name of the current function being compiled, used for labels.
	initialization bool          // Flag to control which list newly compiled code goes into.
	packageName    string        // Current package name.
	unique         int           // Counter for unique labels.
}

type Namespace struct {
	Symbols *SymbolTable
	Types   map[string]Type
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
	c.namespaces = map[string]*Namespace{}
	c.packages = map[string]*Package{}
	c.symbols = newSymbolTable(nil)
	c.cpu = new(DCPU)
	c.alloc = newAllocator(c.cpu.RegCount())
	c.strings = map[string]string{}
	c.types = map[string]Type{}
	c.data = []map[string]*Data{}
	c.globals = map[string]*Data{}
	c.initializers = make([]Outputtable, 0, 256)
	c.code = make([]Outputtable, 0, 4096)
	c.unique = 0
	return c
}

// Returns a new unique label belonging to the current package and function.
func (c *Compiler) packageLabel() string {
	pkg := c.packageName
	if pkg == "." {
		pkg = "main"
	}
	return pkg
}

func (c *Compiler) mkLabel() string {
	c.unique++
	return fmt.Sprintf("%s_%s_%d", c.packageLabel(), c.this, c.unique)
}

func (c *Compiler) namedLabel(base string) string {
	return fmt.Sprintf("%s_%s_%s", c.packageLabel(), c.this, base)
}

func (c *Compiler) locate(name string) *Data {
	for i := len(c.data) - 1; i >= 0; i-- {
		if d, ok := c.data[i][name]; ok {
			return d
		}
	}
	return c.globals[name]
}

// Creates a new data location. Error if one with the same name already exists.
func (c *Compiler) newData(name string, loc *Location) *Data {
	if _, ok := c.data[len(c.data)-1][name]; ok {
		log.Fatalf("Duplicate data definitions for %s", name)
		return nil
	}
	d := &Data{loc: loc}
	c.data[len(c.data)-1][name] = d
	return d
}

// Copy a value into a new location (probably a register). The old location
// becomes the parent of the new one.
// Error if the requested value doesn't already exist.
func (c *Compiler) copyData(name string, loc *Location) *Data {
	d, ok := c.data[name]
	if !ok {
		log.Fatalf("Cannot copy nonexistent data slot %s", name)
	}

	d2 := &Data{loc: loc, parent: d}
	c.data[name] = d2
	return d2
}

// Replaces a data value with its parent. Emits code to write it out, if
// necessary.
// Silently does nothing if a value has no parent. This allows blindly popping
// everything at the end of a block.
func (c *Compiler) popData(name string) {
	d, ok := c.data[name]
	if !ok {
		log.Fatalf("Cannot pop nonexistent data slot %s", name)
	}
	if d.parent == nil {
		return
	}

	if d.dirty {
		c.cpu.Store(d.loc, d.parent.loc, c)
	}
}

func (c *Compiler) compile(o Outputtable) {
	if c.initialization {
		c.initializers = append(c.initializers, o)
	} else {
		c.code = append(c.code, o)
	}
}

func mkScope() *ast.Scope {
	s := ast.NewScope(nil)
	s.Insert(ast.NewObj(ast.Typ, "int"))
	s.Insert(ast.NewObj(ast.Typ, "uint"))
	s.Insert(ast.NewObj(ast.Typ, "string"))
	s.Insert(ast.NewObj(ast.Typ, "bool"))
	s.Insert(ast.NewObj(ast.Typ, "char"))
	s.Insert(ast.NewObj(ast.Con, "false"))
	s.Insert(ast.NewObj(ast.Con, "true"))
	return s
}

// Turns a block of parsed files into the output.
func Compile(files map[string]*ast.File, fset *token.FileSet, libraryPaths []string) []string {
	c := newCompiler()
	c.fset = fset

	// First pass: Build packages across their multiple files.
	pkg, err := ast.NewPackage(fset, files, nil, mkScope())
	if err != nil {
		log.Fatal(err)
	}

	merged := ast.MergePackageFiles(pkg, ast.FilterImportDuplicates)

	// Second pass: For each package, recursively expand its imports.
	// We collect just the top-level packages first, then expand them, since it
	// isn't safe to modify the packages map while iterating over it.
	packages := map[string]*ast.File{".": merged}
	importClosure(merged, fset, libraryPaths, packages)

	// Third pass: Collecting all the global symbols.
	// Take their word for their types for now. Typechecking will catch the liars.
	for pkgName, file := range packages {
		c.packageName = pkgName
		baseName := strings.Split(pkgName, "/")
		pkg := astPackage(file, c)
		syms, types := collectSymbols(pkg)
		c.packages[baseName[len(baseName)-1]] = pkg
		c.namespaces[baseName[len(baseName)-1]] = &Namespace{Symbols: syms, Types: types}
	}

	// Fourth pass: Type checking all the symbols.
	for name, pkg := range c.packages {
		n := c.namespaces[name]
		c.symbols = n.Symbols
		c.types = n.Types
		c.typeCheckAll(pkg)
	}

	fmt.Println("Successfully parsed and typechecked.")

	for name, pkg := range c.packages {
		n := c.namespaces[name]
		c.symbols = n.Symbols
		c.types = n.Types
		pkg.Gen(c)
	}

	fmt.Println("Code generation complete")

	out := []string{}
	for _, o := range c.code {
		out = append(out, o.Output())
	}
	return out
}

// Recursively includes all the packages imported by the given package.
func importClosure(pkg *ast.File, fset *token.FileSet, libraryPaths []string, packages map[string]*ast.File) {
	for _, i := range pkg.Imports {
		pkgPath := i.Path.Value
		if _, ok := packages[pkgPath]; ok {
			continue // Found it already loaded.
		}

		// Otherwise, look for libraryPaths[x] + "/" + i to be a directory.
		importParts := strings.Split(pkgPath, "/") // Always /, not system separators.
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
			importedFiles := map[string]*ast.File{}
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
					g, err := Parse(fset, filename)
					if err != nil {
						log.Fatalf("parse error in imported library %s: %v", i, err)
					}
					importedFiles[filename] = g
				}
			}

			pkg, err := ast.NewPackage(fset, importedFiles, nil, nil)
			if err != nil {
				log.Fatal(err)
			}

			merged := ast.MergePackageFiles(pkg, ast.FilterImportDuplicates)
			packages[i.Name.Name] = merged
			importClosure(merged, fset, libraryPaths, packages)
			break
		}
	}
}

func (c *Compiler) typeError(l Located, format string, args ...interface{}) {
	pos := c.fset.Position(l.Loc()).String()
	args = append([]interface{}{pos}, args...)
	log.Fatalf("%s:  "+format, args...)
}
