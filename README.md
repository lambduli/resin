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
For that reason, I decided to not use it for the time being.


## Structure of the Files

> For more examples, look into the `examples` directory.

The Resin files are meant to have a `.rin` extension.
They might look something like the following snippet:

```
constants: zero .

aliases: 0 = zero
       , 1 = suc(0) .

axioms: ∀ n (Nat(n)) ==> Nat(suc(n))
      , Nat(0) .

theorem 1-is-nat: Nat(1) .

theorem some-nats : ∃ n Nat(n) .

theorem prop-modus-tollens: A => B
                          , ¬B
                          ⊢ ¬A .
```

The syntax is quite simple.
A file is split into three parts:

- `constants`—so that you don't have to write `zero()` and so on,
- `aliases`—a list of non-recursive lexical *rewrite rules*,
- `axioms`—those formulae will be available to all the theorems in the file,
- `theorems`—in general, they are in the shape of *entialment* but you can omit the assumptions part if there aren't any.

All the first three sections must either define one or more constants, axioms or aliases or not be there at all.


## Syntax

The snippet above uses a few unicode characters.
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

> All variables can contain a few special symbols. However, those special symbols may only come after a letter. The special symbols are: `-`, `_`, and `'`. The variable identifiers may technically start with a *digit* character (this is useful for the theorem names).

When we write `∀ x P(f(x))`, the `x` is an *object variable*, the `P` is a *propositional variable* (or rather a constant) and the `f` is a function constant.

The object variables need to begin with a lower-case letter and the propositional variables (names for relations) need to begin with an upper-case letter.

Usually, *functions* share the lexical requirements with object variables—they start with a lower-case letter.

When it comes to *constants* the situation is a little bit more relaxed—a function or a constant defined beforehand (in the `constants` section) can start with both, lower-case and upper-case letters. In such a case, however, a constant must not be written lexically like a nullary function!

There is one more way to modify this behavior—to make writing function identifiers and constants starting with both lower-case and capital-case letters possible and also easier.
There is a special character `ᶜ` that can be written at the end of an identifier that starts with any letter. This makes that identifier be recognized as a constant or a function, depending on the situation, even without it being defined beforehand.

Example: `∃ x Prop(x, Sucᶜ(Zeroᶜ))`

This can be especially useful in the REPL where you can't write a `constants` section. It also makes upper-case functions possible at all.


### Aliases

Only quite restricted alias definitions are allowed. A valid alias is a _constant_ which means it does not have an argument list at the use site—they must be fully applied. At the use site, aliases can not go with the `ᶜ` modifier as that would not make sense. Aliases can be numbers.


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

The prompt has the shape of an entailment, that is by design.

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

10) To put a propositional formula `ƒ` into a *Conjunction Normal Form*, type `:cnf <ƒ>`. However, be aware that this will only really work when the formula does not contain existential quantifiers. Those require skolemization and that leads to only *equisatisfiable* not *equivalent* formulae. The tool will complain a bit but it will produce the *equisatisfiable* formula for the input.


## Q&A

> Q: Why does this exist?

> A: For me, to learn about things, to implement those things (to make sure I learned them right), and to write about them (to make sure that I really got it right), and also to share the experience, knowledge and fun of it.


## Resources

1. [Handbook of Practical Logic and Automated Reasoning](https://www.cl.cam.ac.uk/~jrh13/atp/)

2. [Artificial Intelligence: A Modern Approach](https://aima.cs.berkeley.edu)


The first book is where the source code comes from. It has been only slightly modified to end up being as close to the book's implementation as possible while having a slightly different approach. The book seems to be using the resolution to prove or rather find contradictions in a single formula. This tool handles statements in the form of a logical entailment.

The second book's chapters 7 - 9 contain a lot of information. The whole book is a good starting place for beginners in the topic, however, sometimes the book does not go into much detail regarding some operations—one must use a different resource for that, perhaps the first book.

> In a future, I might write a short write-up about the implementation, all the differences between the code in the book and the one here and how the "trick" from the second book (to produce constructive proofs if applicable) fits into the framework.

> Special Note: The code from the first book seems to not be handling situations where a contradiction (as a formula) is already a part of the original formula. In such cases it seems to me that the `cl` pattern-variable in the function `res'loop` becomes an empty list (empty disjunction, that is equivalent to `⊥`). In those cases the set of resolvents is empty and therefore we don't recognise it as a place to stop. It is clear to me, however, that we should terminate with a found contradiction right at this moment. I will look more into it, hopefully later.


## TODO

- [ ] In all modes (file + REPL) add some flag that causes the following: when a statement can't seem to be proved using the usual algorithm, stop it (after some timeout) and see whether maybe the original goal was already unsatisfiable when conjugated with the assumptions; the flag could be something like `--bidirectional` or `--twoways`
- [ ] In general, implement timeouts
- [x] Lexer
  - [x] `ᶜ` is a special "constant" token
- [ ] Parser
  - [ ] fix the S/R conflicts
  - [x] fix the R/R conflicts
  - [x] LOWER and UPPER followed by `ᶜ` is a constant
  - [x] make sure that the precedences work
  - [x] add support for lexical aliases
    - [x] simple aliases: `0 := zero` and `1 := suc(zero)`
    - [ ] ~~"function" aliases: `1 + _` rewrites to `suc(zero)`~~
          ~~This kinda feels like it could generalize to arbitrary rewrite using some limited grammar. I don't know if I want this. So maybe I'll do with just the simple alias.~~
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
    - [x] unify both implementations (or rather use the one that deals with existential goals) so that when a theorem in the file is existentially quantified and valid, print the assignments that satisfy it
- [x] Support for existential queries (should give constructive answers)
