module Syntax ( Rel(..), Term(..), Formula(..), Theorem(..) ) where


import Prelude hiding ( True, False, And, Or )
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
  show True = "True"
  show False = "False"
  show (Atom (Rel n [])) = n
  show (Atom (Rel n terms)) = n ++ "(" ++ intercalate ", " (map show terms) ++ ")"
  show (Not p) = "¬" ++ show p
  show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
  show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " ==> " ++ show q ++ ")"
  show (Eq p q) = "(" ++ show p ++ " <==> " ++ show q ++ ")"
  show (Forall x p) = "(∀ " ++ x ++ " " ++ show p ++ ")"
  show (Exists x p) = "(∃ " ++ x ++ " " ++ show p ++ ")"


data Theorem = Theorem  { name          :: String
                        , assumptions   :: [Formula]
                        , conclusion    :: Formula }
  deriving (Show, Eq)




