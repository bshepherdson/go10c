This file lists optimizations I have observed that can be made in the generated code:

* Register allocation could still be improved by allowing callers of compileExpr to optionally specify destinations.
* Returns that don't involve using `A` can be optimized to just set the value in `A`.
* Don't `SET PC, _go10c_foo_done` when that label immediately follows (in code for `return`)
* `A, B, C` are being saved before calls, when they need not be. If they're arguments they get saved even if they're not used again later.
* main() can skip the frame pointer stuff at top and bottom. It still needs to make room for locals, though.
* Assignments like `+=` and `-=` can be performed using `ADD` instead of computing and setting separately.
* Comparison operators followed by `if` or `for` conditions can sometimes combine computing a boolean value and then checking it into a single branch instruction.
* Multiple `SET PC, ...` lines in a row can be elided.
* Empty functions can be optimized into a single `SET PC, POP`
* Arguments bound for the same place need not be pushed and popped. Similarly with literals.
* Could probably use SUB and EX to compute <, > etc., instead of four instructions.
* Loop indices, if I can infer them, can be productively stored in registers.

