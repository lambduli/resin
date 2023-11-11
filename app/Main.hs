module Main where


import Prelude hiding ( negate )

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )
import Control.Monad ( mapM_ )
import Data.List qualified as List
import Data.List.Extra qualified as List



import Lexer ( lexer, use'lexer, read'token )
import Parser ( parse'theorems, parse'formula )
import Syntax qualified as S
import Lib ( resolution, {- pure'resolution , -} pure'resolution', pnf, specialize, generalize, a'skolemize, skolemize, simp'cnf, cnf, list'conj, simp'dnf, fv, negate, nnf )


-- from the book:
-- exists x. exists y. forall z.
  --  (  F(x,y) ==> (F(y,z) ∧ F(z,z))  )
-- ∧
-- ((F(x,y) ∧ G(x,y)) ==> (G(x,z) ∧ G(z,z)))

-- This is the formula as understood and printed by the program:
-- (∃ x (∃ y (∀ z (
--                   F(x, y)
--                     ==>
--                   (F(y, z) ∧ F(z, z))
--                 ))))
--   ∧
--  (
--    (F(x, y) ∧ G(x, y))
--      ==> 
--    (G(x, z) ∧ G(z, z))
--  )

-- this is the PNF of the formula
-- (∃ x' (∃ y' (∀ z' ((¬F(x', y') ∨ (F(y', z') ∧ F(z', z'))) ∧ ((¬F(x, y) ∨ ¬G(x, y)) ∨ (G(x, z) ∧ G(z, z)))))))

-- this is the formula after being specialized and in prenex normal form
-- (∃ x' (∃ y' (∀ z' ((¬F(x', y') ∨ (F(y', z') ∧ F(z', z'))) ∧ ((¬F(x, y) ∨ ¬G(x, y)) ∨ (G(x, z) ∧ G(z, z)))))))


example = S.Exists "x"
            (S.Exists "y"
              (S.Forall "z"
                -- F(x, y) ==> (F(y, z) && F(z, z)
                ( S.Atom (S.Rel "F" [S.Var "x", S.Var "y"])
                    `S.Impl`
                  (S.Atom (S.Rel "F" [S.Var "y", S.Var "z"]) `S.And` S.Atom (S.Rel "F" [S.Var "z", S.Var "z"]))
                )
                  --  &&
                  `S.And`
                -- (F(x, y) && G(x, y)) ==> (G(x, z) && G(z, z))
                ( (S.Atom (S.Rel "F" [S.Var "x", S.Var "y"]) `S.And` S.Atom (S.Rel "G" [S.Var "x", S.Var "y"]))
                    `S.Impl`
                  (S.Atom (S.Rel "G" [S.Var "x", S.Var "z"]) `S.And` S.Atom (S.Rel "G" [S.Var "z", S.Var "z"]))
                )
                
                
                ))


-- P(x, y) ==> G(x, y)
--  ∧ P(x, y)
--  ∧ ¬G(x, y)
example1 = ((S.Atom (S.Rel "P" [S.Fn "x" [], S.Fn "y" []])) `S.Impl` (S.Atom (S.Rel "G" [S.Fn "x" [], S.Fn "y" []]))) `S.And` (S.Atom (S.Rel "P" [S.Fn "x" [], S.Fn "y" []])) `S.And` (S.Not (S.Atom (S.Rel "G" [S.Fn "x" [], S.Fn "y" []])))


-- P(x, y) ==> G(x, y)
--  ∧ P(x, y)
--  ∧ G(x, y)
example2 = (((S.Atom (S.Rel "P" [S.Fn "x" [], S.Fn "y" []])) `S.Impl` (S.Atom (S.Rel "G" [S.Fn "x" [], S.Fn "y" []]))) `S.And` (S.Atom (S.Rel "P" [S.Fn "x" [], S.Fn "y" []])) `S.And` ({- S.Not -} (S.Atom (S.Rel "G" [S.Fn "x" [], S.Fn "y" []]))))


-- NOT example2
-- eample two should be a logically valid formula
-- so this should be unsat formula and at least pure'resolution should find the contradiction
-- but pure resolution doesn't
-- it seems to me that the issue is—the negation of the valid formula example2
-- might not actually be an unsat?
-- P(x, y) ∨ ¬G(x, y) ∨ ¬P(x, y) ∨ ¬G(x, y)
-- I mean — this is clearly satisfiable formula.
-- It clearly isn't logically valid, but clearly it's not unsat.
example3 = S.Not example2

-- let's take a look at the example1 again
-- P(x, y) ==> G(x, y) ∧ P(x, y) ∧ ¬G(x, y)
-- ¬P(x, y) ∨ G(x, y) ∧ P(x, y) ∧ ¬G(x, y)
-- 

-- the formula:
-- P(x, y) ==> G(x, y)
--   ∧
-- P(x, y)
--   ∧
-- ¬G(x, y)


-- the PNF of the formula:
-- ¬P(x, y) ∨ G(x, y)
--   ∧
-- P(x, y)
--   ∧
-- ¬G(x, y)


-- specialize and PNF of the formula:
-- ¬P(x, y) ∨ G(x, y)
--   ∧
-- P(x, y)
--   ∧
-- ¬G(x, y)


-- simp'cnf . specialize . pnf of formula:
-- this is a CNF so it's a CONJUNCTION of DISJUNCTIONS
-- { G(x, y) ∨ ¬P(x, y) } && { P(x, y) } && { ¬G(x, y) }


-- the resolution should do:
-- { G(x, y) ∨ ¬P(x, y) } `resolve with` { P(x, y) } giving:
-- { G(x, y) }
-- and that should lead to:
-- { G(x, y) } `resolve with` { ¬G(x, y) } giving:
-- {} aka ⊥


-- (G(x, z) ∧ G(z, z)) ∧ ¬(∀ x (∀ y (∃ z (F(x, y) ∧ (¬F(y, z) ∨ ¬F(z, z))))))
-- (¬F(x, y) ∧ ¬(∀ x (∀ y (∃ z (F(x, y) ∧ (¬F(y, z) ∨ ¬F(z, z)))))))
-- (¬G(x, y) ∧ ¬(∀ x (∀ y (∃ z (F(x, y) ∧ (¬F(y, z) ∨ ¬F(z, z)))))))


-- ¬(∀ x (∀ y (∀ z ((∃ x (∃ y (∀ z (F(x, y) ==> (F(y, z) ∧ F(z, z)))))) ∧ ((F(x, y) ∧ G(x, y)) ==> (G(x, z) ∧ G(z, z)))))))



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


    ':' : 'c' : 'h' : 'e' : 'c' : 'k' : ' ' : file'path -> do
      file'handle <- openFile (List.trim file'path) ReadMode
      file'content <- hGetContents file'handle
      -- let tokens = use'lexer read'token file'content
      -- putStrLn $! "all the tokens:\n" ++ List.intercalate "\n" (map show tokens)
      let theorems = parse'theorems file'content
      mapM_ try'to'prove theorems
      repl assumptions

    ':' : 'c' : 'h' : ' ' : file'path -> do
      file'handle <- openFile (List.trim file'path) ReadMode
      file'content <- hGetContents file'handle
      -- let tokens = use'lexer read'token file'content
      -- putStrLn $! "all the tokens:\n" ++ List.intercalate "\n" (map show tokens)
      let theorems = parse'theorems file'content
      mapM_ try'to'prove theorems
      repl assumptions


    ':' : 'a' : 's' : 's' : 'u' : 'm' : 'e' : ' ' : formula -> do
      let fm = parse'formula formula
      repl $! fm : assumptions

    ':' : 'a' : ' ' : formula -> do
      let fm = parse'formula formula
      repl $! fm : assumptions


    ':' : 's' : 'h' : 'o' : 'w' : _ -> do
      let context = List.intercalate "  ∧  " (map show assumptions)
      putStrLn context
      repl assumptions

    -- ':' : 's' : 'h' : _ -> do
    --   let context = List.intercalate "  ∧  " (map show assumptions)
    --   putStrLn context
    --   repl assumptions

    
    ':' : 'e' : 'n' : 't' : 'a' : 'i' : 'l' : 's' : ' ' : formula -> do
      let fm = parse'formula formula
      try'to'prove'anon assumptions fm
      repl assumptions

    ':' : 'e' : ' ' : formula -> do
      let fm = parse'formula formula
      try'to'prove'anon assumptions fm
      repl assumptions


    ':' : 'c' : 'o' : 'n' : 's' : 'i' : 's' : 't' : 'e' : 'n' : 't' : _ -> do
      let fm = list'conj assumptions
      if pure'resolution' fm
      then do
        putStrLn "❌ the current set of assumptions is not logically consistent"
      else do
        putStrLn "✅ the current set of assumptions is logically consistent"
      repl assumptions

    ':' : 'c' : 'o' : 'n' : _ -> do
      let fm = list'conj assumptions
      if pure'resolution' fm
      then do
        putStrLn "❌ the current set of assumptions is not logically consistent"
      else do
        putStrLn "✅ the current set of assumptions is logically consistent"
      repl assumptions


    ':' : 'c' : 'l' : 'e' : 'a' : 'r' : _ -> do
      repl [S.True]

    -- ':' : 'c' : 'l' : _ -> do
    --   repl [S.True]


    ':' : 's' : 'k' : 'o' : 'l' : 'e' : 'm' : 'i' : 'z' : 'e' : ' ' : formula -> do
      let fm = parse'formula formula
      putStrLn $! show (skolemize fm)
      repl assumptions

    ':' : 's' : 'k' : 'o' : 'l' : ' ' : formula -> do
      let fm = parse'formula formula
      putStrLn $! show (skolemize fm)
      repl assumptions

    
    ':' : 'c' : 'n' : 'f' : ' ' : formula -> do
      let fm = parse'formula formula
      putStrLn $! show (cnf fm)
      repl assumptions


    ':' : 'n' : 'n' : 'f' : ' ' : formula -> do
      let fm = parse'formula formula
      putStrLn $! show (nnf fm)
      repl assumptions

    
    ':' : 'p' : 'n' : 'f' : ' ' : formula -> do
      let fm = parse'formula formula
      putStrLn $! show (pnf fm)
      repl assumptions


    ':' : _ -> do
      putStrLn "I don't know this command, sorry."
      repl assumptions


    input | List.null (List.trim input) -> do
      repl assumptions


    _ -> do
      putStrLn "I don't understand this kind of input, sorry."
      repl assumptions


try'to'prove :: S.Theorem -> IO ()
try'to'prove (S.Theorem { S.name = name
                        , S.assumptions = assumptions
                        , S.conclusion = conclusion }) = do
  -- putStrLn $! "checking theorem `" ++ name ++ "'"
  -- putStrLn $! List.intercalate "\n∧\n" (map show assumptions)
  -- putStrLn $! "⊢ " ++ show conclusion ++ " ."
  let is'valid = resolution assumptions conclusion
  if is'valid
  then do
    putStrLn $! "✅ theorem `" ++ name ++ "' is logically valid"
  else do
    putStrLn $! "❌ theorem `" ++ name ++ "' is not logically valid"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"

  -- putStrLn "________________________________________"


try'to'prove'anon :: [S.Formula] -> S.Formula -> IO ()
try'to'prove'anon assumptions conclusion = do
  let is'valid = resolution assumptions conclusion
  if is'valid
  then do
    putStrLn $! "✅ the conclusion  `" ++ show conclusion ++ "'  is logical consequence of the assumptions"
  else do
    putStrLn $! "❌ the conclusion  `" ++ show conclusion ++ "'  is not logical consequence of the assumptions"
    putStrLn $! "            an interpretation where all the assumptions and `" ++ show (nnf . negate $! conclusion) ++ "' all hold is possible"












  -- putStrLn "Hello, Resin!"
  -- let formula = example3
  -- let pure'result = pure'resolution formula
  -- let pure'result' = pure'resolution' formula
  -- putStrLn "pure resolution first:"
  -- putStrLn $! "the formula: " ++ show formula
  -- putStrLn $! "pnf of formula: " ++ show (pnf formula)
  -- let a = specialize (pnf formula)
  -- putStrLn $! "specialize . pnf $! formula: " ++ show a
  -- let b = simp'cnf (specialize (pnf formula))
  -- putStrLn $! "simp'cnf . specialize . pnf $! formula: " ++ show b
  -- print pure'result
  -- putStrLn "PURE RESOLUTION ' RESULT:"
  -- print pure'result'

  -- putStrLn $! "formula: " ++ show formula
  -- putStrLn $! "S.Not (generalize formula): " ++ show (S.Not (generalize formula))
  -- putStrLn $! "a'skolemize (Not (generalize formula)): " ++ show (a'skolemize (S.Not (generalize formula)))
  -- let fm1 = (a'skolemize (S.Not (generalize formula)))
  -- putStrLn $! "simp'dnf fm1: " ++ show (simp'dnf fm1)
  -- putStrLn $! "map (cnf . list'conj) (simp'dnf formula): " ++ List.intercalate "\n" (map (show . cnf . list'conj) (simp'dnf fm1))
  
  -- let result = resolution formula
  -- putStrLn "resolution now:"
  -- print result



