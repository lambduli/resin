module Syntax ( Rel(..), Term(..), Formula(..), Theorem(..) ) where


import Prelude hiding ( True, False, And, Or )
import Prelude qualified as P
import Data.List ( intercalate )


data Rel = Rel String [Term]            -- P(x, ƒ(x, y))
  deriving (Show, Eq, Ord)


data Term = Var String                  -- x, y, z
          | Fn String [Term]            -- ƒ(x, y)
  deriving (Eq, Ord)


data Formula  = True                    -- ⊤
              | False                   -- ⊥
              | Atom Rel                -- P(x, ƒ(x, y))
              | Not Formula             -- ¬P(x, ƒ(x, y))
              | And Formula Formula     -- ⊤ ∧ ⊤
              | Or Formula Formula      -- ⊤ ∨ ⊥
              | Impl Formula Formula    -- ⊥ ⟹ R
              | Eq Formula Formula      -- F(x) ⟺ G(y)
              | Forall String Formula   -- ∀ x P(x)
              | Exists String Formula   -- ∃ y G(y)
  deriving (Eq, Ord)


instance Show Term where
  show (Var n) = n
  show (Fn n []) = n
  show (Fn n terms) = n ++ "(" ++ intercalate ", " (map show terms) ++ ")"


instance Show Formula where
  show True = "⊤"
  show False = "⊤"
  show (Atom (Rel n [])) = n
  show (Atom (Rel n terms)) = n ++ "(" ++ intercalate ", " (map show terms) ++ ")"
  show (Not p) | is'compound p  = "¬(" ++ show p ++ ")"
  show (Not p) = "¬" ++ show p

  show (And p q)
    | (And _ _) <- p, (And _ _) <- q  = show p ++ " ∧ " ++ show q
    | (And _ _) <- p, is'compound q  = show p ++ " ∧ (" ++ show q ++ ")"
    | (And _ _) <- q, is'compound p  = "(" ++ show p ++ ") ∧ " ++ show q
    | is'compound p, is'compound q  = "(" ++ show p ++ ") ∧ (" ++ show q ++ ")"
    | is'compound p                 = "(" ++ show p ++ ") ∧ " ++ show q
    | is'compound q                 = show p ++ " ∧ (" ++ show q ++ ")"

  show (And p q) = show p ++ " ∧ " ++ show q
  -- show (And p q) = "[" ++ show p ++ " ∧ " ++ show q ++ "]"   -- just leaving this for future debugging
  
  show (Or p q)
    | (Or _ _) <- p, (Or _ _) <- q  = show p ++ " ∨ " ++ show q
    | (Or _ _) <- p, is'compound q  = show p ++ " ∨ (" ++ show q ++ ")"
    | (Or _ _) <- q, is'compound p  = "(" ++ show p ++ ") ∨ " ++ show q
    | is'compound p, is'compound q  = "(" ++ show p ++ ") ∨ (" ++ show q ++ ")"
    | is'compound p                 = "(" ++ show p ++ ") ∨ " ++ show q
    | is'compound q                 = show p ++ " ∨ (" ++ show q ++ ")"
  
  show (Or p q) = show p ++ " ∨ " ++ show q
  -- show (Or p q) = "[" ++ show p ++ "] ∨ [" ++ show q ++ "]"   -- just leaving this for future debugging

  show (Impl p q) = show p ++ " ==> " ++ show q
  -- show (Impl p q) = "(" ++ show p ++ " ==> " ++ show q ++ ")"

  show (Eq p q) = show p ++ " <==> " ++ show q
  -- show (Eq p q) = "(" ++ show p ++ " <==> " ++ show q ++ ")"

  show (Forall x p) = "∀ " ++ x ++ " " ++ show p
  show (Exists x p) = "∃ " ++ x ++ " " ++ show p


is'compound :: Formula -> Bool
is'compound True = P.False
is'compound False = P.False
is'compound (Atom (Rel n [])) = P.False
is'compound (Atom (Rel n terms)) = P.False
is'compound (Not p) = P.True
is'compound (And p q) = P.True
is'compound (Or p q) = P.True
is'compound (Impl p q) = P.True
is'compound (Eq p q) = P.True
is'compound (Forall x p) = P.True
is'compound (Exists x p) = P.True


data Theorem = Theorem  { name          :: String
                        , assumptions   :: [Formula]
                        , conclusion    :: Formula }
  deriving (Show, Eq)
