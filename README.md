# Resin

An implementation of a toy *automated theorem prover* for *classical FOL* built on *resolution*.


## What does this project implement?

TODO: Mention/explain the *given clause* algorithm and give a bit of a background behind *resolution*.


## Usage

TODO: How to build and how to use.


## Q&A

> Q: Why does this exist?

> A: For me to learn about things, to implement those things (to make sure I learned them right), and to write about them (to make sure that I really got it right), and also to share the experience, knowledge and fun.


## Resources

[Handbook of Practical Logic and Automated Reasoning](https://www.cl.cam.ac.uk/~jrh13/atp/)


## TODO

- [x] Lexer
- [ ] Parser
  - [ ] fix the S/R conflicts
  - [ ] fix the R/R conflicts
  - [ ] make sure that the precedences work
- [ ] Printing
  - [ ] only put parens around if necessary
  - [ ] maybe alternate the kinds of parens?
- [ ] REPL
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
