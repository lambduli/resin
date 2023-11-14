{
{-# LANGUAGE FlexibleContexts #-}

module Parser ( parse'module, parse'theorems, parse'formula ) where

import Control.Monad.Error
import Control.Monad.State
import Data.Either.Extra ( mapRight )

import Lexer ( lexer, eval'parser', Lexer(..), AlexInput(..), Lexer'State(..) )
import Token ( Token )
import Token qualified as Token
import Syntax ( Rel(..), Term(..), Formula(..), Theorem(..) )
import Syntax qualified as S

}


%name parseModule Module
%name parseTheorems Theorems
%name parseFormula Formula 

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { Token.EOF }

%errorhandlertype explist
%error { parseError }

%token
  UPPER       { Token.Upper'Var $$ }

  LOWER       { Token.Lower'Var $$ }

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
            :   LOWER                       { [ $1 ] }
            |   LOWER ',' Consts            { $1 : $3 }


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


Formula     ::  { Formula }
            :   '⊤'                         { S.True }
            |   '⊥'                         { S.False }
            |   Relation                    { Atom $1 }
            |   '¬' Formula                 { Not $2 }
            |   Formula '∧' Formula         { $1 `And` $3 }
            |   Formula '∨' Formula         { $1 `Or` $3 }
            |   Formula '==>' Formula       { $1 `Impl` $3 }
            |   Formula '<=>' Formula       { $1 `Eq` $3 }
            |   '∀' Binder QFormula         { Forall $2 $3 } -- TODO: maybe use the scope?
            |   '∃' Binder QFormula         { Exists $2 $3 } -- TODO: maybe use the scope?
            |   '(' Formula ')'             { $2 }
            |   '{' Formula '}'             { $2 }
            |   '[' Formula ']'             { $2 }


Binder      ::  { String }
            :   LOWER                       {% do
                                                { s <- get
                                                ; let binders = scope s
                                                ; put s{ scope = $1 : binders }
                                                ; return $1 } }


QFormula    ::  { Formula }
            :   Formula                     {% do
                                                { s <- get
                                                ; let (binder : binders) = scope s
                                                ; put s{ scope = binders }
                                                ; return $1 } }


Relation    ::  { Rel }
            :   UPPER TermArgsM             { Rel $1 $2 }


TermArgsM   ::  { [Term] }
            :   '(' TArgsSep ')'            { $2 }
            |   {- empty  -}                { [] }


TArgsSep    ::  { [Term] }
            :   Term                        { [ $1 ] }
            |   Term ',' TArgsSep           { $1 : $3 }
            |   {-  empty   -}              { [] }


Term        ::  { Term }
            :   LOWER                       {%  do
                                                { consts <- gets constants
                                                ; binders <- gets scope
                                                ; let is'constant = $1 `elem` consts
                                                ; let is'bound = $1 `elem` binders
                                                ; if is'constant
                                                  then return (Fn $1 [])
                                                  else  if is'bound
                                                        then return (Var $1)
                                                        else throwError $! "Parsing Error: Unbound variable `" ++ $1 ++ "'." } }
            |   LOWER TermArgsM             { Fn $1 $2 }


{

parse'module :: String -> Either String ([String], [Formula], [Theorem])
parse'module source = mapRight fst $! eval'parser' parseModule source

parse'theorems :: String -> Either String  [Theorem]
parse'theorems source = mapRight fst $! eval'parser' parseTheorems source


parse'formula :: String -> Either String Formula
parse'formula source = mapRight fst $! eval'parser' parseFormula source


parseError _ = do
  col'no <- gets (ai'col'no . lexer'input)
  l'no <- gets (ai'line'no . lexer'input)
  state <- get
  throwError $ "Parse error on line " ++ show l'no ++ ", column " ++ show col'no ++ "." -- ++ "  "  ++ show (lexer'input state) -- ++ show state
}
