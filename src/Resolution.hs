module Resolution where

import Sentence ( Sentence(..), Term(..) )
import CNF ( CNF, to'cnf )


type Counter = Int


newtype Used = Used [Sentence CNF]


newtype Unused = Unused [Sentence CNF]


res'loop :: (Used, Unused) -> Counter -> Bool
res'loop (Used used'clauses, Unused unused'clauses) cntr
  = undefined


{-  TODO: It should produce a substitution, shouldn't it?
    So that I can do existentially quantified queries.
    But when I want to do existentially quantified queries I also need to deal with the
    constructive proofs. It would amount to changin the if in the `res'loop` function above.
    Instead of checking whether an empty clause has been derived, I need to check whether
    a clause that contains (I think) exactly those "answer" clauses and then those
    would tell me the correct substitution. So I am gonna deal with that in the future.
  -}

resolution :: [Sentence a] -> Bool
resolution formula = undefined