module CNF where


import Sentence ( Sentence(..) )


{-  A Formula in a CNF is a conjunction/set of Clauses.
    Each Clause is a disjunction/set of Literals or their negation.
-}


--  Negation Normal Form
data NNF


{-  This function replaces all the `_0 <==> _1` with `_0 ==> _1 ∧ _1 ==> _0`
    and all the `_0 ==> _1` with `¬_0 ∨ _1`.
    It then pushes all the `¬_` inwards so that ¬ are only directly in front of
    Literals (Predicates in our case).
  -}

to'nnf :: Sentence a -> Sentence NNF
{-  First, the non-negated cases—equivalences and implications. -}

--  P(x, ..., z) ≡ P(x, ..., z)
to'nnf (Predicate name terms)
  = Predicate name terms

--  A ∧ B ≡ A ∧ B
to'nnf (Conjunction left right)
  = Conjunction (to'nnf left) (to'nnf right)

--  A ∨ B ≡ A ∨ B
to'nnf (Disjunction left right)
  = Disjunction (to'nnf left) (to'nnf right)

--  A ==> B ≡ ¬A ∨ B
to'nnf (Implication premise conclusion)
  = to'nnf (Negation premise) `Disjunction` to'nnf conclusion

--  A <==> B ≡ A ==> B ∧ B ==> A
to'nnf (Equivalence left right)
  = to'nnf (left `Implication` right) `Conjunction` to'nnf (right `Implication` left)

--  ∀ x A ≡ ∀ x A
to'nnf (Forall var sentence)
  = Forall var (to'nnf sentence)

--  ∃ x A ≡ ∃ x A
to'nnf (Exists var sentence)
  = Exists var (to'nnf sentence)

{-  Second, the negation. Pushing negations inwards.  -}

--  ¬P(x, ..., z) ≡ ¬P(x, ..., z)
to'nnf (Negation (Predicate name terms))
  = Negation (Predicate name terms)

--  ¬¬A ≡ A
--  NOTE: This is a Double Negation Elimination! This is only OK in Classical Logic.
--        To make this work for Intuitionistic Logic we keep both negations and handle the sentence within.
to'nnf (Negation (Negation sentence))
  = to'nnf sentence

--  ¬(A ∧ B) ≡ ¬A ∨ ¬B
to'nnf (Negation (Conjunction left right))
  = to'nnf (Negation left) `Disjunction` to'nnf (Negation right)

--  ¬(A ∨ B) ≡ ¬A ∧ ¬B
to'nnf (Negation (Disjunction left right))
  = to'nnf (Negation left) `Conjunction` to'nnf (Negation right)

--  ¬(A ==> B) ≡ ?
to'nnf (Negation s@(Implication _ _))
  = to'nnf (Negation (to'nnf s))

--  ¬(A <==> B) ≡ ?
to'nnf (Negation s@(Equivalence _ _))
  = to'nnf (Negation (to'nnf s))

--  ¬(∀ x A) ≡ ∃ x ¬A
to'nnf (Negation (Forall var sentence))
  = Exists var (to'nnf (Negation sentence))

--  ¬(∃ x A) ≡ ∀ x ¬A
to'nnf (Negation (Exists var sentence))
  = Forall var (to'nnf (Negation sentence))


-- Standardize-Apart Variables
data SAV


to'sav :: Sentence a -> Sentence SAV
to'sav = undefined


-- Skolem Normal Form
data SNF


to'snf :: Sentence SAV -> Sentence SNF
to'snf = undefined


-- No ∀s
data NFA


to'nfa :: Sentence SNF -> Sentence NFA
to'nfa = undefined


-- Conjunctive Normal Form
data CNF


to'cnf :: Sentence NFA -> Sentence CNF
to'cnf = undefined
