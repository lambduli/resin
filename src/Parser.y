{
module Parser ( parse'theorems, parse'formula ) where

import Control.Monad.Error
import Control.Monad.State

import Lexer ( lexer, eval'parser, Lexer(..) )
import Token ( Token )
import Token qualified as Token
import Syntax ( Rel(..), Term(..), Formula(..), Theorem(..) )
import Syntax qualified as S

}


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

%%

-- TODO: Continue from here!

Theorems    ::  { [Theorem] }
            :   Theorem Theorems            { $1 : $2 }
            |   {-  empty   -}              { [] }


Theorem     ::  { Theorem }
            :   'theorem' LOWER ':' Assumptions '⊢'  Conclusion '.'
                                            { Theorem { name = $2
                                                      , assumptions = $4
                                                      , conclusion = $6 } }


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
            |   '∀' LOWER Formula           { Forall $2 $3 } -- TODO: maybe use the scope?
            |   '∃' LOWER Formula           { Exists $2 $3 } -- TODO: maybe use the scope?
            |   '(' Formula ')'             { $2 }
            |   '{' Formula '}'             { $2 }
            |   '[' Formula ']'             { $2 }


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
            :   LOWER                       { Var $1 }
            |   LOWER TermArgsM             { Fn $1 $2 }


{

parse'theorems :: String -> [Theorem]
parse'theorems source = eval'parser parseTheorems source


parse'formula :: String -> Formula
parse'formula source = eval'parser parseFormula source


parseError _ = do
  -- col'no <- gets (inpColumn . lexerInput)
  -- l'no <- gets (inpLine . lexerInput)
  -- state <- get
  error $ "Parse error on line " -- ++ show l'no ++ ", column " ++ show col'no ++ "." ++ "  " ++ show state
}
