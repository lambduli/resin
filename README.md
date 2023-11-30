# Resin

An implementation of a toy *automated theorem prover* for *classical FOL* built on *resolution*.


## What does this project implement?

The implementation is based on a *given clause algorithm* for *resolution*.
For more information about it, see the *Resources* section.


The [book](https://www.cl.cam.ac.uk/~jrh13/atp/) in the *Resources*
describes some optimizations as well as specific forms of resolution.
This implementation does not (yet) implement those specific forms or resolution.
It does, however, contain an implementation of the optimization the book mentions.
The subsumption, to be more precise. As it turns how, however,
that was making the implementation really slow, unbearably slow.
For that reason I decided to not use it for the time being.


## Structure of the Files

> For more examples, look into the `examples` directory.

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

The syntax is quite simple.
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
| `∨`     | `OR`, `\|\|`, `\|` |
| `¬`     | `NOT`     |
| `⟹`     | `==>`, `=>` |
| `⟺`     | `<=>`, `<==>` |


### Identifiers

Resin differentiates between two types of identifiers—the ones used mainly for *object variables* and the ones used for *propositional variables* and for functions.

> All variables can contain a few special symbols. However, those special symbols may only come after a letter. The special symbols are: `-`, `_`, `'`, and *any digit*.

When we write `∀ x P(f(x))`, the `x` is an *object variable*, the `P` is a *propositional variable* and the `f` is a function constant.

The object variables need to begin with lower-case letter and the propositional variables (names for relations) need to begin with upper-case letter.

Usually, *functions* share the lexical requirements with object variables—they start with lower-case letter.

When it comes to *constants* the situation is little bit more relaxed—a function or a constant defined beforehand (in the `constants` section) can start with both, lower-case and upper-case letter. In such a case, however, constant must not be written lexically like a nullary function!

There is one more way to modify this behaviour—to make writing function identifiers and constants starting with both lower-case and capital-case letters possible and also easier.
There is a special character `ᶜ` that can be written at the end of an identifier that starts with any letter. This makes that identifier be recognized as a constant or a function, depending on the situation, even without it being defined beforehand.

Example: `∃ x Prop(x, Sucᶜ(Zeroᶜ))`

This can be especially useful in the REPL where you can't write a `constants` section. It also makes upper-case functions possible at all.


### Numbers

To make some propositions look nicer *Resin* also supports numbers as constant symbols.
In files, they can be defined in the `constants` section, like this: `constants: 1, 2, 3 ...`.
In the REPL one can also use the `ᶜ` trick like so: `0ᶜ`.


### Quantifiers

If there's a sequence of the same kind of quantifier like `∀ x ∀ y ∀ z ...` you can shorten it to just `∀ x y z ...`.


## Usage

To build and run, type `cabal build` and `cabal run`.


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

10) To put a propositional formula `ƒ` into a *Conjunction Normal Form*, type `:cnf <ƒ>`. However, be aware that this will only work when the formula does not contain existential quantifiers. Those require skolemization and that leads to only *equisatisfiable* not *equivalent* formulae. The tool will reject formulae with `∃` with `:cnf` command.


## Q&A

> Q: Why does this exist?

> A: For me, to learn about things, to implement those things (to make sure I learned them right), and to write about them (to make sure that I really got it right), and also to share the experience, knowledge and fun of it.


## Resources

1. [Handbook of Practical Logic and Automated Reasoning](https://www.cl.cam.ac.uk/~jrh13/atp/)

2. [Artificial Intelligence: A Modern Approach](https://aima.cs.berkeley.edu)


The first book is the where the source code comes from. It has been only slightly modified to end up being as close to the book's implementation as possible.

The second book's chapters 7 - 9 contain a lot of information. The whole book is a good starting place for beginners in the topic, however, sometimes the book does not go into much details regarding some operations—one must use different resource for that, perhaps the first book.


## TODO

- [x] Lexer
  - [x] `ᶜ` is a special "constant" toke
- [ ] Parser
  - [ ] fix the S/R conflicts
  - [x] fix the R/R conflicts
  - [x] LOWER and UPPER followed by `ᶜ` is a constant
  - [x] make sure that the precedences work
- [x] Printing
  - [x] only put parens around if necessary
- [x] REPL
  - [x] `:assume` command that inserts a Formula into a live set of assumptions (the list of assumptions is initialized with `⊤` so the prompt looks like `⊤ ⊢`)
  - [x] `:entails` command that check whether the current list of assumptions entails a given formula
  - [x] `:consistent` command check that the current set of assumptions does not contain a contradiction
  - [x] `:clear` comand clears the assumptions (sets it to just one `⊤`)
  - [ ] `:load` command to load axioms and constants from a file (will require storing the parser-state and reusing it)
  - [x] the command over formula mode
    - [x] `:skolemize` command
    - [x] `:pnf` command
    - [x] `:nnf` command
    - [x] `:cnf` command
  - [x] the theorem prover mode
    - [x] normal mode
    - [x] verbose mode
    - [x] unify both implementations (or rather use the one that deals with existential goals) so that when a theorem in file is existentially quantified and valid, print the assignments that satisfy it
- [x] Support for existential queries (should give constructive answers)
