module Main where


import Prelude hiding ( negate )

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )
import System.Environment ( getArgs )
import Control.Monad ( unless )
import Data.List qualified as List
import Data.List.Extra qualified as List


import Parser ( parse'formula, parse'module )
import Syntax qualified as S
import Given'Clause ( resolution, pure'resolution, resolution', pren'norm'form, skol'norm'form, con'norm'form, negate, neg'norm'form, nnf, contains'exists, list'conj )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Resin — a toy automated theorem prover for classical First Order Logic."
      repl [S.True]
      putStrLn "Bye!"
    _ -> do
      mapM_ check args


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
      check file'path
      repl assumptions

    ':' : 'c' : 'h' : ' ' : 'v' : 'e' : 'r' : ' ' : file'path -> do
      check'verbose assumptions file'path

    ':' : 'c' : 'h' : ' ' : file'path -> do
      check file'path
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
      find (prompt'len + 9) assumptions formula

    ':' : 'e' : ' ' : formula -> do
      find (prompt'len + 3) assumptions formula

    ':' : 'f' : 'i' : 'n' : 'd' : ' ' : formula -> do
      find (prompt'len + 6) assumptions formula

    ':' : 'c' : 'o' : 'n' : 's' : 'i' : 's' : 't' : 'e' : 'n' : 't' : _ -> do
      consistent assumptions

    ':' : 'c' : 'o' : 'n' : _ -> do
      consistent assumptions

    ':' : 'c' : 'l' : 'e' : 'a' : 'r' : _ -> do
      repl [S.True]

    
    {-  The following commands are mostly for debugging formulae.   -}
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

    ':' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : formula -> do
      case parse'formula formula of
        Left (err, col) -> do
          let padding = take (prompt'len + 8 + col) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
        Right fm -> do
          putStrLn $! show fm
      repl assumptions
    {-  END of debugging commands.  -}

    ':' : _ -> do
      putStrLn "I don't know this command, sorry."
      repl assumptions

    input | List.null (List.trim input) -> do
      repl assumptions

    --  Because what the prompt looks like, the `entails` check is the default.
    formula -> do
      find prompt'len assumptions formula


try'to'prove :: [S.Formula] -> S.Theorem -> IO ()
try'to'prove axioms (S.Theorem{ S.name = name
                              , S.assumptions = assumptions
                              , S.conclusion = conclusion }) = do
  case resolution' (axioms ++ assumptions) conclusion of
    Just _ -> do
      putStrLn $! "✅ theorem `" ++ name ++ "' is logically valid"
    Nothing -> do
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
  case resolution' (axioms ++ assumptions) conclusion of
    Just _ -> do
      putStrLn $! "✅ theorem `" ++ name ++ "' is logically valid"
    Nothing -> do
      putStrLn $! "❌ theorem `" ++ name ++ "' is not logically valid"
      putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"
  putStrLn ""


check :: String -> IO ()
check file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left (err, _) -> do
      putStrLn err
    Right (_, axioms, theorems) -> do
      mapM_ (try'to'prove axioms) theorems


check'verbose :: [S.Formula] -> String -> IO ()
check'verbose assumptions file'path = do
  file'handle <- openFile (List.trim file'path) ReadMode
  file'content <- hGetContents file'handle
  case parse'module file'content of
    Left (err, _) -> do
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


find :: Int -> [S.Formula] -> String -> IO ()
find prompt'len assumptions formula = do
  case parse'formula formula of
    Left (err, col) -> do
      let padding = take (prompt'len + col) $! repeat ' '
      putStrLn $! padding ++ "^"
      putStrLn err
    Right conclusion -> do
      case resolution assumptions conclusion of
        Nothing -> do
          let pad = List.take prompt'len $! List.repeat ' '
          putStrLn $! "❌ `" ++ show conclusion ++ "'  is not a logical consequence of the assumptions"
          putStrLn $! pad ++ " an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"
        Just answers -> do
          putStrLn $! "✅ the conclusion  `" ++ show conclusion ++ "'  is a logical consequence of the assumptions"
          let assignment = map (\ (exis, term) -> "   for `" ++ exis ++ "' being `" ++ show term ++ "'") answers
          unless (List.null assignment) $! putStrLn $! List.intercalate ", " assignment
  repl assumptions


consistent :: [S.Formula] -> IO ()
consistent assumptions = do
  let fm = list'conj assumptions
  case pure'resolution fm of
    Just _ -> do
      putStrLn "❌ the current set of assumptions is not logically consistent"
    Nothing -> do
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
      if contains'exists fm
      then do
        putStrLn "⚠️  I can't really perform the CNF conversion on a FOL formula containing an existential quantifier."
        putStrLn "   This would require skolemization, a process that would produce only an equisatisfiable formula."
        putStrLn "   The following is a result of doing so, be warned!"
      else do
        return ()
      putStrLn $! show (con'norm'form fm)
  repl assumptions
