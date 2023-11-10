# TODO

- [ ] Lexer
- [ ] Parser
- [ ] REPL
  - [ ] the theorem prover mode
  - [ ] the command over formula mode
- [ ] Support for existential queries (should give constructive answers)


## Representation of the FOL Formulae

There are *Terms*.
Those are quantifiable *variables* like `x` and `y`, and *functions*.
Functions can be nullary—those don't take any arguments. We call them constants.

There are *FOL Atomic Propositions*.
Those represent relations.
They can have one or multiple *Terms* for arguments or they can be nullary.
The nullary ones are *Propositional Variables*. Or maybe rather *constants*.
They are some non-parametrized propositions. Like *sky is blue* or so.

There are also *logical connectives*.
However, we can make an argument that we don't really need those as
a formula in CNF is a collection of *disjuncts*.
Those, in turn are collections of *FOL Atomic Propositions*.
So we could do without representing connectives in our *core* data type.

