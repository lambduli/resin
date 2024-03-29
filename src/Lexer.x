{
module Lexer ( lexer, read'token, eval'parser, Lexer(..), AlexInput(..), Lexer'State(..) ) where


import Control.Monad.Except ( Except, runExcept, throwError )
import Control.Monad.State ( MonadState(get, put), gets, StateT( runStateT ) )

import Data.Word ( Word8 )
import Data.Char ( ord )
import Data.List ( uncons )

import Token ( Token )
import Token qualified as Token

import Syntax ( Term )

}

%encoding "iso-8859-1"


$upper                = [A-Z]

$lower                = [a-z]

$digit                = [0-9]

@lowerident           = [$lower $digit] [$lower \- \_ $digit \']*

@upperident           = $upper [$lower $upper \- \_ $digit \']*

@number               = [$digit]+

$space                = [\ \t\f\v\n]


resin :-

$space+                 ;

"%".*\n                 ;

"ᶜ"                     { \_ -> token Token.Constant'Before }

"("                     { \_ -> token Token.Paren'Open }
")"                     { \_ -> token Token.Paren'Close }
"["                     { \_ -> token Token.Box'Open }
"]"                     { \_ -> token Token.Box'Close }
"{"                     { \_ -> token Token.Bracket'Open }
"}"                     { \_ -> token Token.Bracket'Close }

-- keywords
","                     { \_ -> token Token.Comma }
"."                     { \_ -> token Token.Period }
"theorem"               { \_ -> token Token.Theorem }
"constants"             { \_ -> token Token.Constants }
"axioms"                { \_ -> token Token.Axioms }
"aliases"               { \_ -> token Token.Aliases }
":"                     { \_ -> token Token.Colon }
"⊢"                     { \_ -> token Token.Turnstile }

"="                     { \_ -> token Token.Equal }

-- logical connectives
"⊤"                     { \_ -> token Token.Tautology }
"TRUE"                  { \_ -> token Token.Tautology }
"True"                  { \_ -> token Token.Tautology }
"Tautology"             { \_ -> token Token.Tautology }
"TAUTOLOGY"             { \_ -> token Token.Tautology }


"⊥"                     { \_ -> token Token.Contradiction }
"FALSE"                 { \_ -> token Token.Contradiction }
"FALSE"                 { \_ -> token Token.Contradiction }
"CONTRADICTION"         { \_ -> token Token.Contradiction }
"Contradiction"         { \_ -> token Token.Contradiction }

"∀"                     { \_ -> token Token.Forall }
"forall"                { \_ -> token Token.Forall }

"∃"                     { \_ -> token Token.Exists }
"exists"                { \_ -> token Token.Exists }

"¬"                     { \_ -> token Token.Negate }
"NOT"                   { \_ -> token Token.Negate }

"∧"                     { \_ -> token Token.And }
"&&"                    { \_ -> token Token.And }
"&"                     { \_ -> token Token.And }
"AND"                   { \_ -> token Token.And }

"∨"                     { \_ -> token Token.Or }
"||"                    { \_ -> token Token.Or }
"|"                     { \_ -> token Token.Or }
"OR"                    { \_ -> token Token.Or }

"=>"                    { \_ -> token Token.Implication }
"==>"                   { \_ -> token Token.Implication }
"⟹"                     { \_ -> token Token.Implication }

"<=>"                   { \_ -> token Token.Equivalence }
"<==>"                  { \_ -> token Token.Equivalence }
"⟺"                     { \_ -> token Token.Equivalence }



@lowerident             { emit Token.Lower'Var }

@upperident             { emit Token.Upper'Var }

@number                 { emit Token.Number }


{


token :: Token -> Lexer Token
token t = return t


emit :: (String -> Token) -> String -> Lexer Token
emit mk't str = return (mk't str)


lexer :: (Token -> Lexer a) -> Lexer a
lexer cont = read'token >>= cont


read'token :: Lexer Token
read'token = do
  s <- get
  case alexScan (lexer'input s) 0 of
    AlexEOF -> return Token.EOF

    AlexError inp' ->
      throwError ("Lexical error on line " ++ (show $! ai'line'no inp') ++ " and column " ++ (show $! ai'col'no inp'), ai'col'no inp')
    
    AlexSkip inp' _ -> do
      put s{ lexer'input = inp' }
      read'token
    
    AlexToken inp' n act -> do
      let (Input{ ai'input = buf }) = lexer'input s
      put s{ lexer'input = inp' }
      act (take n buf)


-- The functions that must be provided to Alex's basic interface
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@Input{ ai'input }
  = advance <$> uncons ai'input
    where
      advance :: (Char, String) -> (Word8, AlexInput)
      advance ('\n', rest)
        = ( fromIntegral (ord '\n')
          , Input { ai'line'no    = ai'line'no input + 1
                  , ai'col'no     = 1
                  , ai'last'char  = '\n'
                  , ai'input      = rest } )

      advance ('∨', rest)
        = ( fromIntegral (ord '|')
          , Input { ai'line'no    = ai'line'no input
                  , ai'col'no     = ai'col'no input + 1
                  , ai'last'char  = '|'
                  , ai'input      = rest } )
        
      advance (c, rest)
        = ( fromIntegral (ord c)
          , Input { ai'line'no    = ai'line'no input
                  , ai'col'no     = ai'col'no input + 1
                  , ai'last'char  = c
                  , ai'input      = rest } )


get'line'no :: Lexer Int
get'line'no = gets (ai'line'no . lexer'input)


get'col'no :: Lexer Int
get'col'no = gets (ai'col'no . lexer'input)


eval'parser :: Lexer a -> String -> Either (String, Int) (a, Lexer'State)
eval'parser parser source = runExcept $! runStateT parser (initial'state source)


type Lexer a = StateT Lexer'State (Except (String, Int)) a


data AlexInput = Input
  { ai'line'no   :: !Int
  , ai'col'no    :: !Int
  , ai'last'char :: !Char
  , ai'input     :: String }
  deriving (Eq, Show)


data Lexer'State = Lexer'State
  { lexer'input   :: !AlexInput
  , constants     :: ![String]
  , scope         :: ![String]
  , aliases       :: ![(String, Term)] }
  deriving (Eq, Show)


initial'state :: String -> Lexer'State
initial'state s = Lexer'State
  { lexer'input       = Input
                        { ai'line'no    = 1
                        , ai'col'no     = 1 
                        , ai'last'char  = '\n'
                        , ai'input      = s }
  , constants         = []
  , scope             = []
  , aliases           = [] }

}
