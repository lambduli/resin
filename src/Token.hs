module Token ( Token(..) ) where


data Token  = Upper'Var String
            | Lower'Var String
            | Number String

            | Constant'Before

            --  keywords
            | Comma           --  ,
            | Period          --  .
            | Theorem         --  theorem
            | Axioms          --  axioms
            | Constants       --  constants
            | Aliases         --  aliases
            | Colon           --  :
            | Turnstile       --  ⊢

            | Tautology       --  ⊤
            | Contradiction   --  ⊥
            | Forall          --  ∀
            | Exists          --  ∃
            | Negate          --  ¬ NOT
            | And             --  ∧ && AND
            | Or              --  ∨ || OR
            | Implication     --  ==>
            | Equivalence     --  <=>

            | Paren'Open      --  (
            | Paren'Close     --  )
            | Box'Open        --  [
            | Box'Close       --  ]
            | Bracket'Open    --  {
            | Bracket'Close   --  }

            | Underscore      --  _
            | Equal           --  =
            | EOF
  deriving (Show, Eq)
