# Resin

An implementation of a toy *automated theorem prover* for *classical FOL* built on *resolution*.


## What does this project implement?

The implementation is based on a *given clause algorithm* for *resolution*.
For more information about it, see the *Resources* section.


## Structure of the Files

The Resin files are meant to have a `.rin` extension.
They might look something like the following snippet:

```
constants: zero .

axioms: ∀ n (Nat(n)) ==> Nat(suc(n))
      , Nat(zero) .

theorem one-is-nat: Nat(suc(zero)) .

theorem some-nats : ∃ n Nat(n) .

theorem prop-modus-tollens: A => B
                          , ¬B
                          ⊢ ¬A .
```

The grammar is quite simple.
A file is split in three parts:

- `constants`—so that you don't have to write `zero()` and so on,
- `axioms`—those formulae will be available to all the theorems in the file,
- `theorems`—in general, they are in the shape of *entialment* but you can omit the assumptions part if there aren't any.

Both `constants` and `axioms` sections must either define one or more constants or axioms or not be there at all.


## Syntax

The snippet above uses a few unicode character.
You don't have to use those, the following shows the groups of symbols with the same meaning:

| Unicode | ASCII     |
|---------|-----------|
| `∀`     | `forall`  |
| `∃`     | `exists`  |
| `⊤`     | `True`, `Tautology` |
| `⊥`     | `False`, `Contradiction` |
| `∧`     | `AND`, `&&` |
| `∨`     | `OR`, `\|\|` |
| `¬`     | `NOT`     |
| `⟹`     | `==>`     |
| `⟺`     | `<=>`     |


## Usage

To build type `cabal build` and to run `cabal run`.

## The Interface of the REPL

1) To check all the theorems in a file, type `:check <file-path>`.

The prompt has a shape of an entailment, that is by design.

2) To assert that a particular formula `ƒ` is an assumption, type `:assume <ƒ>`.

The prompt is kept short for convenience.

3) To print all the assumptions, type `:show`.

4) To see if a formula `ƒ` is entailed by the current set of assumptions, type `:entails <ƒ>` or just enter the formula directly. That is why the prompt has that shape.

5) To check that the current set of assumptions is consistent (does not contain a contradiction), type `:consistent`.

6) To reset the current set of assumptions back to `⊤`, type `:clear`.

There are also a few commands for a simple transformation on formulae.

7) To put a formula `ƒ` into a *Negation Normal Form*, type `:nnf <ƒ>`.

8) To put a formula `ƒ` into a *Prenex Normal Form*, type `:pnf <ƒ>`.

9) To put a formula `ƒ` into a *Skolem Normal Form*, type `:skolemize <ƒ>`.

10) To put a propositional formula `ƒ` into a *Conjunction Normal Form*, type `:cnf <ƒ>`.


## Q&A

> Q: Why does this exist?

> A: For me to learn about things, to implement those things (to make sure I learned them right), and to write about them (to make sure that I really got it right), and also to share the experience, knowledge and fun.


## Resources

[Handbook of Practical Logic and Automated Reasoning](https://www.cl.cam.ac.uk/~jrh13/atp/)

The source code has been extracted from the book and only slightly modified.


## TODO

- [x] Lexer
- [ ] Parser
  - [ ] fix the S/R conflicts
  - [ ] fix the R/R conflicts
  - [x] make sure that the precedences work
- [x] Printing
  - [x] only put parens around if necessary
- [x] REPL
  - [x] `:assume` command that inserts a Formula into a live set of assumptions (the list of assumptions is initialized with `⊤` so the prompt looks like `⊤ ⊢`)
  - [x] `:entails` command that check whether the current list of assumptions entails a given formula
  - [x] `:consistent` command check that the current set of assumptions does not contain a contradiction
  - [x] `:clear` comand clears the assumptions (sets it to just one ⊤)
  - [x] the command over formula mode
    - [x] `:skolemize` command
    - [x] `:pnf` command
    - [x] `:nnf` command
    - [x] `:cnf` command
  - [x] the theorem prover mode
    - [x] normal mode
- [ ] Support for existential queries (should give constructive answers)
