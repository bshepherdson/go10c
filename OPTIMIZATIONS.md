This file lists optimizations I have observed that can be made in the generated code:

* Register allocation could still be improved by allowing callers of compileExpr to optionally specify destinations.
    - Returns that don't involve using `A` can be optimized to just set the value in `A`.
* Don't `SET PC, _go10c_foo_done` when that label immediately follows (in code for `return`)
* main() can skip the frame pointer stuff at top and bottom. It still needs to make room for locals, though.
* Assignments like `+=` and `-=` can be performed using `ADD` instead of computing and setting separately.
* Arguments bound for the same place need not be pushed and popped. Similarly with literals.
    - Only if they are not used in the computation of later arguments. Needs more sophisticated data flow analysis.
* Could probably use SUB and EX to compute <, > etc., instead of four instructions.
    - This may well be more complicated than it's worth.
* Loop indices, if I can infer them, can be productively stored in registers.

