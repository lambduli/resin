module Formula where


import Data.Set qualified as Set


import Syntax ( Rel )


data Literal  = Atom Rel
              | Not Rel
  deriving (Show, Eq)


{-  Clause is a Disjunction of Literals.  -}
{-  Maybe, instead of Disjunction use `Some'Of`.  -}
newtype Clause = Disjunction (Set.Set Literal)
  deriving (Show, Eq)


{-  Formula is a Conjunction of Clauses.  -}
{-  Maybe, instead of Conjunction use `All`'Of`.  -}
data Formula = Conjunction (Set.Set Clause)
             | Contradiction
  deriving (Show, Eq)
