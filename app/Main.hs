module Main where


import Prelude hiding ( negate )

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )
import Control.Monad ( mapM_ )
import Data.List qualified as List
import Data.List.Extra qualified as List


import Lexer ( lexer, {- use'lexer, -} read'token )
import Parser ( parse'theorems, parse'formula, parse'module )
import Syntax qualified as S
import Lib ( resolution, pure'resolution', pren'norm'form, pnf, specialize, a'skolemize, skol'norm'form, simp'cnf, con'norm'form, list'conj, simp'dnf, fv, negate, neg'norm'form, nnf, fol'formula )


main :: IO ()
main = do
  putStrLn "Resin — a toy automated theorem prover for classical First Order Logic."
  repl [S.True]
  putStrLn "Bye!"


repl :: [S.Formula] -> IO ()
repl assumptions = do
  let context = List.intercalate ", " (map show assumptions)
  let short'context = (List.take 15 context) ++ if List.length context > 15 then "..." else ""
  putStr $! short'context ++ " ⊢ "
  hFlush stdout
  str <- getLine
  case str of
    ":q" -> return ()
    ":Q" -> return ()

    ':' : 'c' : 'h' : 'e' : 'c' : 'k' : ' ' : 'v' : 'e' : 'r' : 'b' : 'o' : 's' : 'e' : ' ' : file'path -> do
      check'verbose assumptions file'path

    ':' : 'c' : 'h' : 'e' : 'c' : 'k' : ' ' : file'path -> do
      check assumptions file'path

    ':' : 'c' : 'h' : ' ' : 'v' : 'e' : 'r' : ' ' : file'path -> do
      check'verbose assumptions file'path

    ':' : 'c' : 'h' : ' ' : file'path -> do
      check assumptions file'path

    ':' : 'a' : 's' : 's' : 'u' : 'm' : 'e' : ' ' : formula -> do
      assume assumptions formula

    ':' : 'a' : ' ' : formula -> do
      assume assumptions formula

    ':' : 's' : 'h' : 'o' : 'w' : _ -> do
      let context = List.intercalate "  ∧  " (map show assumptions)
      putStrLn context
      repl assumptions
    
    ':' : 'e' : 'n' : 't' : 'a' : 'i' : 'l' : 's' : ' ' : formula -> do
      entails assumptions formula

    ':' : 'e' : ' ' : formula -> do
      entails assumptions formula

    ':' : 'c' : 'o' : 'n' : 's' : 'i' : 's' : 't' : 'e' : 'n' : 't' : _ -> do
      consistent assumptions

    ':' : 'c' : 'o' : 'n' : _ -> do
      consistent assumptions

    ':' : 'c' : 'l' : 'e' : 'a' : 'r' : _ -> do
      repl [S.True]

    ':' : 's' : 'k' : 'o' : 'l' : 'e' : 'm' : 'i' : 'z' : 'e' : ' ' : formula -> do
      skolemize assumptions formula

    ':' : 's' : 'k' : 'o' : 'l' : ' ' : formula -> do
      skolemize assumptions formula

    ':' : 's' : 'i' : 'm' : 'p' : ' ' : formula -> do
      case parse'formula formula of
        Left err -> do
          putStrLn err
        Right fm -> do
          putStrLn $! show fm
      repl assumptions
    
    ':' : 'c' : 'n' : 'f' : ' ' : formula -> do
      cnf assumptions formula

    ':' : 'n' : 'n' : 'f' : ' ' : formula -> do
      case parse'formula formula of
        Left err -> do
          putStrLn err
        Right fm -> do
          putStrLn $! show (neg'norm'form fm)
      repl assumptions

    ':' : 'p' : 'n' : 'f' : ' ' : formula -> do
      case parse'formula formula of
        Left err -> do
          putStrLn err
        Right fm -> do
          putStrLn $! show (pren'norm'form fm)
      repl assumptions

    -- ':' : 't' : 'o' : 'k' : ' ' : input -> do
    --   let tokens = use'lexer read'token input
    --   putStrLn $! "All the tokens:\n" ++ List.intercalate "\n  " (map show tokens)
    --   repl assumptions

    ':' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : formula -> do
      case parse'formula formula of
        Left err -> do
          putStrLn err
        Right fm -> do
          putStrLn $! show fm
      repl assumptions

    ':' : _ -> do
      putStrLn "I don't know this command, sorry."
      repl assumptions

    input | List.null (List.trim input) -> do
      repl assumptions

    --  Because what the prompt looks like, the `entails` check is the default.
    formula -> do
      entails assumptions formula


try'to'prove :: [S.Formula] -> S.Theorem -> IO ()
try'to'prove axioms (S.Theorem{ S.name = name
                              , S.assumptions = assumptions
                              , S.conclusion = conclusion }) = do
  let is'valid = resolution (axioms ++ assumptions) conclusion
  if is'valid
  then do
    putStrLn $! "✅ theorem `" ++ name ++ "' is logically valid"
  else do
    putStrLn $! "❌ theorem `" ++ name ++ "' is not logically valid"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"


try'to'prove'verbose :: [S.Formula] -> S.Theorem -> IO ()
try'to'prove'verbose axioms (S.Theorem{ S.name = name
                              , S.assumptions = assumptions
                              , S.conclusion = conclusion }) = do
  let first'line = "checking theorem `" ++ name ++ "':"
  putStr $! first'line
  let pad'len = List.length first'line - 1
  let pad = List.take pad'len $! List.repeat ' '
  putStrLn $! ' ' : List.intercalate ('\n' : pad ++ ", ") (map show (axioms ++ assumptions))
  putStrLn $! pad ++ "⊢ " ++ show conclusion ++ " ."
  let is'valid = resolution (axioms ++ assumptions) conclusion
  if is'valid
  then do
    putStrLn $! "✅ theorem `" ++ name ++ "' is logically valid"
  else do
    putStrLn $! "❌ theorem `" ++ name ++ "' is not logically valid"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"
  putStrLn ""


try'to'prove'anon :: [S.Formula] -> S.Formula -> IO ()
try'to'prove'anon assumptions conclusion = do
  let is'valid = resolution assumptions conclusion
  if is'valid
  then do
    putStrLn $! "✅ the conclusion  `" ++ show conclusion ++ "'  is logical consequence of the assumptions"
  else do
    putStrLn $! "❌ the conclusion  `" ++ show conclusion ++ "'  is not logical consequence of the assumptions"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"


check :: [S.Formula] -> String -> IO ()
check assumptions file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left err -> do
      putStrLn err
    Right (_, axioms, theorems) -> do
      mapM_ (try'to'prove axioms) theorems
  repl assumptions


check'verbose :: [S.Formula] -> String -> IO ()
check'verbose assumptions file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left err -> do
      putStrLn err
    Right (_, axioms, theorems) -> do
      mapM_ (try'to'prove'verbose axioms) theorems
  repl assumptions


assume :: [S.Formula] -> String -> IO ()
assume assumptions formula = do
  case parse'formula formula of
    Left err -> do
      putStrLn err
      repl assumptions
    Right fm -> do
      repl $! fm : assumptions


entails :: [S.Formula] -> String -> IO ()
entails assumptions formula = do
  case parse'formula formula of
    Left err -> do
      putStrLn err
    Right fm -> do
      try'to'prove'anon assumptions fm
  repl assumptions


consistent :: [S.Formula] -> IO ()
consistent assumptions = do
  let fm = list'conj assumptions
  if pure'resolution' fm
  then do
    putStrLn "❌ the current set of assumptions is not logically consistent"
  else do
    putStrLn "✅ the current set of assumptions is logically consistent"
  repl assumptions


skolemize :: [S.Formula] -> String -> IO ()
skolemize assumptions formula = do
  case parse'formula formula of
    Left err -> do
      putStrLn err
    Right fm -> do
      putStrLn $! show (skol'norm'form fm)
  repl assumptions


cnf :: [S.Formula] -> String -> IO ()
cnf assumptions formula = do
  case parse'formula formula of
    Left err -> do
      putStrLn err
    Right fm -> do
      if fol'formula fm
      then do
        putStrLn "⚠️  I can't perform the CNF conversion on a non-propositional formula."
        putStrLn "   The formula contains existential quantifiers."
        putStrLn "   This would require skolemization, a process that might produce only a equisatisfiable formula."
      else do
        putStrLn $! show (con'norm'form fm)
      repl assumptions