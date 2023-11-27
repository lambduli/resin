{
{-# LANGUAGE FlexibleContexts #-}

module Parser ( parse'module, parse'theorems, parse'formula, parse'two'formulae ) where

import Control.Monad.Except ( throwError )
import Control.Monad.State ( MonadState(get, put), gets )
import Data.Either.Extra ( mapRight )
import Data.List qualified as List
import Data.Maybe ( fromMaybe )

import Lexer ( lexer, eval'parser, Lexer(..), AlexInput(..), Lexer'State(..) )
import Token ( Token )
import Token qualified as Token
import Syntax ( Rel(..), Term(..), Formula(..), Theorem(..) )
import Syntax qualified as S

}


%name parseModule Module
%name parseTheorems Theorems
%name parseFormula Formula
%name parseTwoFormulae Formulae

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { Token.EOF }

%errorhandlertype explist
%error { parseError }

%token
  UPPER       { Token.Upper'Var $$ }

  LOWER       { Token.Lower'Var $$ }

  NUMBER      { Token.Number $$ }

  'ᶜ'         { Token.Constant'Before }

  ','         { Token.Comma }
  '.'         { Token.Period }
  'theorem'   { Token.Theorem }
  'constants' { Token.Constants }
  'axioms'    { Token.Axioms }
  ':'         { Token.Colon }
  '⊢'         { Token.Turnstile }
  '⊤'         { Token.Tautology }
  '⊥'         { Token.Contradiction }
  '∀'         { Token.Forall }
  '∃'         { Token.Exists }
  '¬'         { Token.Negate }
  '∧'         { Token.And }
  '∨'         { Token.Or }
  '==>'       { Token.Implication }
  '<=>'       { Token.Equivalence }
  '('         { Token.Paren'Open }
  ')'         { Token.Paren'Close }
  '['         { Token.Box'Open }
  ']'         { Token.Box'Close }
  '{'         { Token.Bracket'Open }
  '}'         { Token.Bracket'Close }

  '_'         { Token.Underscore }
  '='         { Token.Equal }


%right '<=>'
%right '==>'
%left '∨'
%left '∧'
%right '¬'

%%

Module      ::  { ([String], [Formula], [Theorem]) }
            :   Constants Axioms Theorems   { ($1, $2, $3) }


Constants   ::  { [String] }
            :   'constants' ':' Consts '.'  {%  do
                                                { s <- get
                                                ; put s{ constants = $3 }
                                                ; return $3 } }
            |   {-  empty   -}              { [] }


Consts      ::  { [String] }
            :   Constant                    { [ $1 ] }
            |   Constant ',' Consts         { $1 : $3 }


Constant    ::  { String }
            :   LOWER                       { $1 }
            |   UPPER                       { $1 }
            |   NUMBER                      { $1 }


Axioms      ::  { [Formula] }
            :   'axioms' ':' Axs '.'        { $3 }
            |   {-  empty   -}              { [] }


Axs         ::  { [Formula] }
            :   Formula                     { [ $1 ] }
            |   Formula ',' Axs             { $1 : $3 }


Theorems    ::  { [Theorem] }
            :   Theorem Theorems            { $1 : $2 }
            |   {-  empty   -}              { [] }


Theorem     ::  { Theorem }
            :   'theorem' LOWER ':' Assumptions '⊢'  Conclusion '.'
                                            { Theorem { name = $2
                                                      , assumptions = $4
                                                      , conclusion = $6 } }
            |   'theorem' LOWER ':' Formula '.'
                                            { Theorem { name = $2
                                                      , assumptions = []
                                                      , conclusion = $4 } }


Assumptions ::  { [Formula] }
            :   Formula AssumpsRest         { $1 : $2 }
            |   {-  empty   -}              { [] }

AssumpsRest ::  { [Formula] }
            :   ',' Formula AssumpsRest     { $2 : $3 }
            |   {-  empty   -}              { [] }


Conclusion  ::  { Formula }
            :   Formula                     { $1 }


Formulae    ::  { (Formula, Formula) }
            :   Formula ',' Formula         { ($1, $3) }


Formula     ::  { Formula }
            :   '⊤'                         { S.True }
            |   '⊥'                         { S.False }
            |   Relation                    { Atom $1 }
            |   '¬' Formula                 { Not $2 }
            |   Formula '∧' Formula         { $1 `And` $3 }
            |   Formula '∨' Formula         { $1 `Or` $3 }
            |   Formula '==>' Formula       { $1 `Impl` $3 }
            |   Formula '<=>' Formula       { $1 `Eq` $3 }
            |   '∀' Binders QFormula        { List.foldl' (flip Forall) $3 (List.reverse $2) }  --  NOTE: The Binders will never return an empty list, so don't worry.
            |   '∃' Binders QFormula        { List.foldl' (flip Exists) $3 (List.reverse $2) }  --  NOTE: The Binders will never return an empty list, so don't worry.
            |   '(' Formula ')'             { $2 }
            |   '{' Formula '}'             { $2 }
            |   '[' Formula ']'             { $2 }


Binders     ::  { [String] }
            :   LOWER                       {% do
                                                { s <- get
                                                ; let binders = scope s
                                                ; put s{ scope = $1 : binders }
                                                ; return [ $1 ] } }
            |   LOWER Binders               {% do
                                                { s <- get
                                                ; let binders = scope s
                                                ; put s{ scope = $1 : binders }
                                                ; return ($1 : $2) } }


QFormula    ::  { Formula }
            :   Formula                     {% do
                                                { s <- get
                                                ; let (binder : binders) = scope s
                                                ; put s{ scope = binders }
                                                ; return $1 } }


Relation    ::  { Rel }
            :   UPPER TermArgsM             { case $2 of
                                              { Just terms -> Rel $1 terms
                                              ; Nothing -> Rel $1 [] } }


TermArgsM   ::  { Maybe [Term] }
            :   '(' TArgsSep ')'            { Just $2 }
            |   '[' TArgsSep ']'            { Just $2 }
            |   '{' TArgsSep '}'            { Just $2 }
            |   {- empty  -}                { Nothing }


TArgsSep    ::  { [Term] }
            :   Term                        { [ $1 ] }
            |   Term ',' TArgsSep           { $1 : $3 }
            |   {-  empty   -}              { [] }


Term        ::  { Term }
            :   NUMBER                      {%  do
                                                { consts <- gets constants
                                                ; col'now <- gets (ai'col'no . lexer'input)
                                                ; l'no <- gets (ai'line'no . lexer'input)
                                                ; let col'no = col'now - List.length $1
                                                ; if $1 `elem` consts
                                                  then return (Fn $1 [])
                                                  else throwError ("Parsing Error: Unknown numeric constant `" ++ $1 ++ "' on line " ++ show l'no ++ " column " ++ show col'no ++ ".", col'no) } }
            |   NUMBER 'ᶜ'                  { Fn $1 [] }
            |   LOWER TermArgsM             {%  case $2 of
                                                { Just terms -> return $! Fn $1 terms
                                                ; Nothing -> do
                                                  { consts <- gets constants
                                                  ; binders <- gets scope
                                                  ; col'now <- gets (ai'col'no . lexer'input)
                                                  ; l'no <- gets (ai'line'no . lexer'input)
                                                  ; let col'no = col'now - List.length $1
                                                  ; let is'constant = $1 `elem` consts
                                                  ; let is'bound = $1 `elem` binders
                                                  ; if is'constant
                                                    then return (Fn $1 [])
                                                    else  if is'bound
                                                          then return (Var $1)
                                                          else throwError ("Parsing Error: Unbound variable `" ++ $1 ++ "' on line " ++ show l'no ++ " column " ++ show col'no ++ ".", col'no) } } }
            
            |   LOWER 'ᶜ' TermArgsM         { Fn $1 (fromMaybe [] $3) }
            |   UPPER TermArgsM             {%  case $2 of
                                                { Just terms -> return $! Fn $1 terms
                                                ; Nothing -> do
                                                  { consts <- gets constants
                                                  ; col'now <- gets (ai'col'no . lexer'input)
                                                  ; l'no <- gets (ai'line'no . lexer'input)
                                                  ; let col'no = col'now - List.length $1
                                                  ; if $1 `elem` consts
                                                    then return (Fn $1 [])
                                                    else throwError ("Parsing Error: Unknown constant `" ++ $1 ++ "' on line " ++ show l'no ++ " column " ++ show col'no ++ ".", col'no) } } }
            |   UPPER 'ᶜ' TermArgsM         { Fn $1 (fromMaybe [] $3) }
            |   '(' Term ')'                { $2 }


{

parse'module :: String -> Either (String, Int) ([String], [Formula], [Theorem])
parse'module source = mapRight fst $! eval'parser parseModule source

parse'theorems :: String -> Either (String, Int)  [Theorem]
parse'theorems source = mapRight fst $! eval'parser parseTheorems source


parse'formula :: String -> Either (String, Int) Formula
parse'formula source = mapRight fst $! eval'parser parseFormula source


parse'two'formulae :: String -> Either (String, Int) (Formula, Formula)
parse'two'formulae source = mapRight fst $! eval'parser parseTwoFormulae source


parseError _ = do
  col'no <- gets (ai'col'no . lexer'input)
  l'no <- gets (ai'line'no . lexer'input)
  last'char <- gets (ai'last'char . lexer'input)
  state <- get
  throwError ("Parse error near character `" ++ [last'char] ++ "' on line " ++ show l'no ++ ", column " ++ show col'no ++ ".", col'no)
}
