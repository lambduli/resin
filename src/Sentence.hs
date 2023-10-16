module Sentence ( Sentence(..), Term(..) ) where


data Sentence a = Predicate String [Term]
                | Negation (Sentence a)
                | Conjunction (Sentence a) (Sentence a)
                | Disjunction (Sentence a) (Sentence a)
                | Implication (Sentence a) (Sentence a)
                | Equivalence (Sentence a) (Sentence a)
                | Forall String (Sentence a)
                | Exists String (Sentence a)
  deriving (Show, Eq)


data Term = Function String [Term]
          | Constant String
          | Variable String
  deriving (Show, Eq)
