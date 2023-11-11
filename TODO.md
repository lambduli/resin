# TODO

- [x] Lexer
- [ ] Parser
- [ ] Printing
  - [ ] only put parens around if necessary
  - [ ] maybe alternate the kinds of parens?
- [ ] REPL
  - [ ] fix the S/R conflicts
  - [ ] fix the R/R conflicts
  - [ ] make sure that the precedences work
  - [x] `:assume` command that inserts a Formula into a live set of assumptions (the list of assumptions is initialized with `⊤` so the prompt looks like `⊤ ⊢`)
  - [x] `:entails` command that check whether the current list of assumptions entails a given formula
  - [x] `:consistent` command check that the current set of assumptions does not contain a contradiction
  - [x] `:clear` comand clears the assumptions (sets it to just one ⊤)
  - [x] the command over formula mode
    - [x] `:skolemize` command
    - [x] `:pnf` command
    - [x] `:nnf` command
    - [x] `:cnf` command
  - [ ] the theorem prover mode
    - [x] normal mode
    - [ ] When checking a theorem, check that the assumptions themselves do not lead to a contradiction.
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

