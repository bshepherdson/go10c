For this I will break the spec that forbids casting to/from pointers and allow casting from a `uint` to any pointer type. Therefore we can handle linking by importing a Go library that has a function for each system call, which creates a pointer to a specific place in CubeOS, a fixed location in low memory.

That memory location contains a pointer to a table of function addresses. That table in assembly and the functions in Go must be kept in sync. Functions may be added, but never rearranged or moved, or programs compiled previously will fail to function.

The Go functions will load the value at that point, add an offset for the given function, and cast the result into a function pointer of the appropriate type. Finally, it will call the function, passing along the arguments and returning its result if applicable. In this way Go functions can call CubeOS functions.

This approach is sufficient for linking against system calls, but is not a general linking scheme with a foreign symbol table. That may come at some point in the future.
