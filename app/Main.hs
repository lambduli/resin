module Main where


import Prelude hiding ( negate )

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )
import System.Environment ( getArgs )
import Control.Monad ( mapM_ )
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.Set qualified as Set


import Lexer ( lexer, {- use'lexer, -} read'token )
import Parser ( parse'theorems, parse'formula, parse'module, parse'two'formulae )
import Syntax qualified as S
import Lib ( resolution, resolution'', pure'resolution', pren'norm'form, pnf, specialize, skolemise, a'skolemize, skol'norm'form, simp'cnf, con'norm'form, list'conj, simp'dnf, fv, negate, neg'norm'form, nnf, features'exists, resolve'clauses, list'disj, list'conj, generalize )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Resin — a toy automated theorem prover for classical First Order Logic."
      repl [S.True]
      putStrLn "Bye!"
    _ -> do
      mapM_ (check [S.True]) args
      -- repl [S.True]


repl :: [S.Formula] -> IO ()
repl assumptions = do
  let context = List.intercalate ", " (map show assumptions)
  let short'context = (List.take 15 context) ++ if List.length context > 15 then " ..." else ""
  let prompt'len = List.length short'context + 3 - 1
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
      repl assumptions

    ':' : 'c' : 'h' : ' ' : 'v' : 'e' : 'r' : ' ' : file'path -> do
      check'verbose assumptions file'path

    ':' : 'c' : 'h' : ' ' : file'path -> do
      check assumptions file'path
      repl assumptions

    ':' : 'a' : 's' : 's' : 'u' : 'm' : 'e' : ' ' : formula -> do
      assume (prompt'len + 8) assumptions formula

    ':' : 'a' : ' ' : formula -> do
      assume (prompt'len + 3) assumptions formula

    ':' : 's' : 'h' : 'o' : 'w' : _ -> do
      let context = List.intercalate "  ∧  " (map show assumptions)
      putStrLn context
      repl assumptions
    
    ':' : 'e' : 'n' : 't' : 'a' : 'i' : 'l' : 's' : ' ' : formula -> do
      entails (prompt'len + 9) assumptions formula

    ':' : 'e' : ' ' : formula -> do
      entails (prompt'len + 3) assumptions formula

    ':' : 'f' : 'i' : 'n' : 'd' : ' ' : formula -> do
      find (prompt'len + 6) assumptions formula

    ':' : 'c' : 'o' : 'n' : 's' : 'i' : 's' : 't' : 'e' : 'n' : 't' : _ -> do
      consistent assumptions

    ':' : 'c' : 'o' : 'n' : _ -> do
      consistent assumptions

    ':' : 'c' : 'l' : 'e' : 'a' : 'r' : _ -> do
      repl [S.True]

    ':' : 's' : 'k' : 'o' : 'l' : 'e' : 'm' : 'i' : 'z' : 'e' : ' ' : formula -> do
      skolemize (prompt'len + 11) assumptions formula

    ':' : 's' : 'k' : 'o' : 'l' : ' ' : formula -> do
      skolemize (prompt'len + 6) assumptions formula

    ':' : 's' : 'i' : 'm' : 'p' : ' ' : formula -> do
      case parse'formula formula of
        Left (err, col) -> do
          let padding = take (prompt'len + 6 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
        Right fm -> do
          putStrLn $! show fm
      repl assumptions
    
    ':' : 'c' : 'n' : 'f' : ' ' : formula -> do
      cnf (prompt'len + 5) assumptions formula

    ':' : 'n' : 'n' : 'f' : ' ' : formula -> do
      case parse'formula formula of
        Left (err, col) -> do
          let padding = take (prompt'len + 5 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
        Right fm -> do
          putStrLn $! show (neg'norm'form fm)
      repl assumptions

    ':' : 'p' : 'n' : 'f' : ' ' : formula -> do
      case parse'formula formula of
        Left (err, col) -> do
          let padding = take (prompt'len + 5 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
        Right fm -> do
          putStrLn $! show (pren'norm'form fm)
      repl assumptions

    ':' : 'r' : 'e' : 's' : 'o' : 'l' : 'v' : 'e' : ' ' : formulae -> do
      case parse'two'formulae formulae of
        Left (err, col) -> do
          let padding = take (prompt'len + 5 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
        Right (fm1, fm2) -> do
          let cl1 = simp'cnf . specialize . skolemise $! fm1
          let cl2 = simp'cnf . specialize . skolemise $! fm2

          let resolvents = Set.toList $! Set.map list'disj $! resolve'clauses (List.head cl1) (List.head cl2)
          putStrLn $! List.intercalate "\n" (map (show . generalize) resolvents)
      repl assumptions


    -- ':' : 't' : 'o' : 'k' : ' ' : input -> do
    --   let tokens = use'lexer read'token input
    --   putStrLn $! "All the tokens:\n" ++ List.intercalate "\n  " (map show tokens)
    --   repl assumptions

    ':' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : formula -> do
      case parse'formula formula of
        Left (err, col) -> do
          let padding = take (prompt'len + 8 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
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
      entails prompt'len assumptions formula


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
    putStrLn $! "✅ the conclusion  `" ++ show conclusion ++ "'  is a logical consequence of the assumptions"
  else do
    putStrLn $! "❌ the conclusion  `" ++ show conclusion ++ "'  is not a logical consequence of the assumptions"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"


check :: [S.Formula] -> String -> IO ()
check assumptions file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left (err, col) -> do
      putStrLn err
    Right (_, axioms, theorems) -> do
      mapM_ (try'to'prove axioms) theorems


check'verbose :: [S.Formula] -> String -> IO ()
check'verbose assumptions file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left (err, col) -> do
      putStrLn err
    Right (_, axioms, theorems) -> do
      mapM_ (try'to'prove'verbose axioms) theorems
  repl assumptions


assume :: Int -> [S.Formula] -> String -> IO ()
assume prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
      repl assumptions
    Right fm -> do
      repl $! fm : assumptions


entails :: Int -> [S.Formula] -> String -> IO ()
entails prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
    Right fm -> do
      try'to'prove'anon assumptions fm
  repl assumptions


find :: Int -> [S.Formula] -> String -> IO ()
find prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
    Right conclusion -> do
      -- try'to'prove'anon assumptions fm
      case resolution'' assumptions conclusion of
        Nothing -> do
          putStrLn $! "❌ no such objects found, `" ++ show conclusion ++ "'  is not a logical consequence of the assumptions"
          putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"
        Just answers -> do
          putStrLn $! "✅ the conclusion  `" ++ show conclusion ++ "'  is a logical consequence of the assumptions"
          let assignment = map (\ (exis, term) -> "   for `" ++ exis ++ "' being `" ++ show term ++ "'") answers
          putStrLn $! List.intercalate ", " assignment
          -- putStrLn $! show answers
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


skolemize :: Int -> [S.Formula] -> String -> IO ()
skolemize prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
    Right fm -> do
      putStrLn $! show (skol'norm'form fm)
  repl assumptions


cnf :: Int -> [S.Formula] -> String -> IO ()
cnf prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
    Right fm -> do
      if features'exists fm
      then do
        putStrLn "⚠️  I can't perform the CNF conversion on a non-propositional formula."
        putStrLn "   The formula contains existential quantifiers."
        putStrLn "   This would require skolemization, a process that might produce only a equisatisfiable formula."
        putStrLn $! show (con'norm'form fm)
      else do
        putStrLn $! show (con'norm'form fm)
  repl assumptions