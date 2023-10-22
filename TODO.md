# TODO

- [ ] Conversion to CNF
- [ ] *Given Clause Algorithm*


## The Algorithm as described in the Book

What we need:

- a function that renames all bound variables in a clause (to unique names)
- a function that unifies all the literals in a list/set together; fails or produces a *substitution*
- a function that tests whether two literals are unifiable
- a function that produces a collection of resolvents for given *two clauses*
- a function that drives the resolution—it start with a bunch of clauses in `unused` and none at `used` and does the *given clause algorithm*
- a function that takes a bunch of clauses (a formula) and transforms them into CNF
- the main function that delegates all the work
