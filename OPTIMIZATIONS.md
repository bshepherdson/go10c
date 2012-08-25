This file lists optimizations I have observed that can be made in the generated code:

* Do not decrement (and increment) the stack pointer to make room for locals when there are no locals, because `SUB/ADD SP, 0` is a no-op.
* Allocation of registers in expressions is less than ideal.
    * Literal values don't need to be placed in new registers, but should be folded into the following instruction.
    * It will copy from argument registers to the expression return registers before computing their values.
    * Fixing that is much more difficult than it seems at first, though.
* Don't `SET PC, _go10c_foo_done` when that label immediately follows (in code for `return`)
* `A, B, C` are being saved in functions, when they need not be. If they're arguments they get saved even if they're not used again later.
* main() can skip the frame pointer stuff at top and bottom. It still needs to make room for locals, though.
* Plenty of `SET A, A`s are being generated.
* Consider a last-minute pass over the assembly to remove known no-ops like `SET A, A` and `ADD/SUB A, 0` and others.
    * To do this kind of optimization, things would be much simpler with an ADT for the assembly. I should do that transformation.
* Assignments like `+=` and `-=` can be performed using `ADD` instead of computing and setting separately.
* Comparison operators followed by `if` or `for` conditions can combine computing a boolean value and then checking it into a single branch instruction.
* Multiple `SET PC, ...` lines in a row can be elided.
* Negative integer literals could be compiled directly and/or optimized into one instruction instead of 3.
* Empty functions can be optimized into a single `SET PC, POP`

