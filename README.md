# go10c

go10c is a compiler from a dialect of Google Go to DCPU-16 assembly. It is written in Haskell.

## Details

* The output is intended to be fed to [das](https://github.com/jonpovey/das), but most DCPU-16 version 1.7-compliant assemblers that use Notch's syntax (rather than gas syntax) will work.
* Assembly quirks:
    - Uses the `HCF` "halt and catch fire" instruction.
    - Uses the `DAT` pseudoinstruction that compiles literal data into the binary, with values separated by commas and allowing string literals (`DAT "example", 0` inserts a C string literal, including the null terminator, one ASCII value per 16-bit word).
    - Does **not** rely on das's complex constant expressions.

## Differences from standard Go

* No garbage collection. See below about `new` and `delete`.
* The only types are:
    - `uint` (16-bit unsigned integer)
    - `int` (16-bit 2's complement signed integer)
    - `bool` (true/false value. 0 is false, nonzero is true)
    - `char` (ASCII value in the bottom 7 bits of a `uint`)
        - character literals support only `'a'`, `'\n'` and friends, '\777' (octal) and `'\x34'` (hex). They do not support `'\u340f'` or `'\Udeadbeef'` style, since there is no Unicode support.
    - `string` (pointer to a C-style null terminated string, using char
    - arrays, written `[]T` where `T` is one of these types, including an array type. This is an atomic type in the compiler, implemented behind the scenes as a pointer to memory. It can be indexed, moved around, assigned. Not copied on function calls like in standard Go!
    - pointers to any of these types, including pointers. Written `*T` where `T` is a type. Pointer values are copied on function calls, so modifying the pointer itself won't change the original.
* Function calls use the standard "registercall" calling convention: clobbering `A`, `B`, `C`, preserving the other general purpose registers, but not `EX`/`O`. Extra arguments are on the stack, followed by the return address. Return address is at `[SP]`, fourth argument at `[SP+1]`, fifth at `[SP+2]`, etc. Callers are responsible for cleaning up the stack arguments. Return values in `A`, stack unchanged by the callee upon its return. Nothing is specified for allocating or freeing of memory.
    - As an aside for CubeOS, we currently don't follow that convention internally; our functions preserve, and expect to be preserved, all non-argument registers. These functions will be safe to call from Go functions. Go functions can be safely called from assembly if care is taken to obey this difference of convention.
* Go functions can have named return values, and multiple return values. Neither of these is supported.
* Go functions will have a label with their name and package basename, thus: _package_function. Internal label names will be globally unique and unpredictable. Using them in any way other than the compiler does is undefined.
* `new(T)` is reappropriated to create a new value of the given type `T` on the heap, which is *not* garbage collected.
    * `new([]T, n)` creates a new array with elements of type `T` and length `n`.
* `delete(x)` frees the value `x` which was previously created with `new()`. This is not the same meaning as standard Go `delete()`.The behavior when `free()`ing something that was not allocated with `new()`, or has already been freed, is undefined. Nasal demons.
* `panic()` calls the DCPU-16 `HCF` "halt and catch fire" instruction.
* Methods, interfaces, closures, function pointers, slices, maps, and all concurrency constructs are left out for now. Possibly in a future version. I realize this is all the interesting bits, but so be it.
* Newly allocated values, local and global values **are not** set to 0. Do not rely on them.
* Global variables may not have initializers; they will be ignored.
* Execution begins at `main()`. A file may be compiled that has no `main()`; in that case it is a library, and an attempt to execute the file will make the DCPU halt and catch fire.
* Constants are unsupported for now.
* Struct changes:
    - Tags are not supported.
    - Anonymous fields are not supported.
* Variadic functions and the `...` syntax for them are not supported.
* Types cannot be omitted in variable declarations, even when unambiguous default values are given.
* Struct literals must have their keys provided. Array literals do not support indexes.
* Taking the address of a literal, including a struct or array literal, is not supported.
* `range` clauses on for loops are not supported.

Some of these restrictions may be lifted in future versions.

