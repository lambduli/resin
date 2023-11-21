module Lib where

import Prelude hiding ( negate )
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Foldable ( Foldable(foldl') )
import Data.Map.Strict qualified as Map


import Syntax ( Rel(..), Term(..), Formula(Atom, Not, And, Or, Impl, Eq, Forall, Exists) )
import Syntax qualified as S


import Debug.Trace ( trace )


over'atoms :: (Rel -> b -> b) -> Formula -> b -> b
over'atoms f (Atom rel) b = f rel b
over'atoms f (Not p) b = over'atoms f p b
over'atoms f (And p q) b = over'atoms f p (over'atoms f q b)
over'atoms f (Or p q) b = over'atoms f p (over'atoms f q b)
over'atoms f (Impl p q) b = over'atoms f p (over'atoms f q b)
over'atoms f (Eq p q) b = over'atoms f p (over'atoms f q b)
over'atoms f (Forall x p) b = over'atoms f p b
over'atoms f (Exists x p) b = over'atoms f p b


atom'union :: Ord a => (Rel -> Set.Set a) -> Formula -> Set.Set a
atom'union f fm = over'atoms (\ rel acc -> f rel `Set.union` acc) fm Set.empty


p'simplify' :: Formula -> Formula
p'simplify' (Not S.False) = S.True
p'simplify' (Not S.True) = S.False
p'simplify' (Not (Not p)) = p
p'simplify' (And _ S.False) = S.False
p'simplify' (And S.False _) = S.False
p'simplify' (And p S.True) = p
p'simplify' (And S.True p) = p
p'simplify' (Or p S.False) = p
p'simplify' (Or S.False p) = p
p'simplify' (Impl S.False _) = S.True
p'simplify' (Impl _ S.True) = S.True
p'simplify' (Impl S.True p) = p
p'simplify' (Impl p S.False) = Not p
p'simplify' (Eq p S.True) = p
p'simplify' (Eq S.True p) = p
p'simplify' (Eq p S.False) = Not p
p'simplify' (Eq S.False p) = Not p
p'simplify' f = f


negative :: Formula -> Bool
negative (Not _) = True
negative _ = False


positive :: Formula -> Bool
positive = not . negative


negate :: Formula -> Formula
negate (Not p) = p
negate p = Not p


list'conj :: [Formula] -> Formula
list'conj [] = S.True
list'conj (f : fs) = foldl' And f fs


list'disj :: [Formula] -> Formula
list'disj [] = S.False
list'disj (f : fs) = foldl' Or f fs


{-  The Disjunction Normal Form   -}


distrib :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a) -> Set.Set (Set.Set a)
distrib s1 s2 = Set.map (uncurry Set.union) (Set.cartesianProduct s1 s2)


{-  Just a small test.  -}
-- test1 :: Bool
-- test1 = distrib (Set.fromList [Set.fromList ["P"], Set.fromList ["Q", "R"]])
--                 (Set.fromList [Set.fromList ["¬P"], Set.fromList ["¬R"]])
--         ==
--         Set.fromList  [ Set.fromList ["P", "¬P"]
--                       , Set.fromList ["P", "¬R"]
--                       , Set.fromList ["Q", "R", "¬P"]
--                       , Set.fromList ["Q", "R", "¬R"] ]


pure'dnf :: Formula -> [[Formula]]
pure'dnf fm = map Set.toList $! Set.toList $! pure'dnf' fm


pure'dnf' :: Formula -> Set.Set (Set.Set Formula)
pure'dnf' (And p q) = distrib (pure'dnf' p) (pure'dnf' q)
pure'dnf' (Or p q) = pure'dnf' p `Set.union` pure'dnf' q
pure'dnf' fm = Set.singleton (Set.singleton fm)


trivial :: [Formula] -> Bool
trivial literals =
  let (pos, neg) = List.partition positive literals
  in  not (List.null (pos `List.intersect` List.map negate neg))
{-  TODO: In the book they use a function named `image` instead of `List.map`.
          The function `image` is just a map but on Sets.
          It is my understadning that if the list of literals was already a "Set"
          then the `negs` will also be a "Set" and the `negate` operation
          should not really make it not to be.
          But ideally, I would want to rewrite the code to use real Sets instead of Lists.
-}


simp'dnf :: Formula -> [[Formula]]
simp'dnf S.False = []
simp'dnf S.True = [[]]
simp'dnf fm =
  let djs           = filter (not . trivial) (pure'dnf (nnf fm))
      djs'sets      = map Set.fromList djs
      simp'djs'sets = filter (\ d -> not $! List.any (`Set.isProperSubsetOf` d) djs'sets) djs'sets
  in  map Set.toList simp'djs'sets


{-  Disjunction Normal Form   -}
dnf :: Formula -> Formula
dnf fm = list'disj $! map list'conj $! simp'dnf fm


{-  The Conjunction Normal Form   -}


{-  NOTE: There is a notable trickery going on.
          We start with a negation of the original formula
          that is what we transform into a NNF and then DNF.
          We then go and negate every Literal within every Disjunct of the whole formula.

          The book does this so that the transformation does not explode the formula
          into a huge proportions.          
-}
pure'cnf :: Formula -> [[Formula]]
pure'cnf fm = image (image negate) (pure'dnf (nnf (Not fm)))
  where
    image :: (Ord a, Ord b) => (a -> b) -> [a] -> [b]
    image fn l = Set.toList $! Set.map fn (Set.fromList l)


simp'cnf :: Formula -> [[Formula]]
simp'cnf S.False = [[]]
simp'cnf S.True = []
simp'cnf fm =
  let cjs           = filter (not . trivial) (pure'cnf fm)
      cjs'sets      = map Set.fromList cjs
      simp'cjs'sets = filter (\ c -> not $! List.any (`Set.isProperSubsetOf` c) cjs'sets) cjs'sets
  in  map Set.toList simp'cjs'sets


{-  Conjunction Normal Form   -}
cnf :: Formula -> Formula
cnf fm = list'conj $! map list'disj $! simp'cnf fm


{-  Predicate Logic   -}


fvt :: Term -> Set.Set String
fvt (Var n) = Set.singleton n
fvt (Fn _ terms) = Set.unions (map fvt terms)


fv :: Formula -> Set.Set String
fv S.False = Set.empty
fv S.True = Set.empty
fv (Atom (Rel _ terms)) = Set.unions (map fvt terms)
fv (Not p) = fv p
fv (And p q) = Set.union (fv p) (fv q)
fv (Or p q) = Set.union (fv p) (fv q)
fv (Impl p q) = Set.union (fv p) (fv q)
fv (Eq p q) = Set.union (fv p) (fv q)
fv (Forall x p) = Set.delete x (fv p)
fv (Exists x p) = Set.delete x (fv p)


generalize :: Formula -> Formula
generalize fm = List.foldr Forall fm (fv fm)


t'subst :: Map.Map String Term -> Term -> Term
t'subst subst (Var x) =  Map.findWithDefault (Var x) x subst
t'subst subst (Fn f terms) = Fn f (map (t'subst subst) terms)


variant :: String -> Set.Set String -> String
variant x vars
  = if x `Set.member` vars
    then variant (x ++ "'") vars
    else x


subst :: Map.Map String Term -> Formula -> Formula
subst sub S.False = S.False
subst sub S.True = S.True
subst sub (Atom (Rel p terms)) = Atom (Rel p $! map (t'subst sub) terms)
subst sub (Not p) = Not (subst sub p)
subst sub (And p q) = And (subst sub p) (subst sub q)
subst sub (Or p q) = Or (subst sub p) (subst sub q)
subst sub (Impl p q) = Impl (subst sub p) (subst sub q)
subst sub (Eq p q) = Eq (subst sub p) (subst sub q)
subst sub (Forall x p) = subst'q sub Forall x p
subst sub (Exists x p) = subst'q sub Exists x p


subst'q :: Map.Map String Term -> (String -> Formula -> Formula) -> String -> Formula -> Formula
subst'q sub quant x p
  = let x' =  if List.any (\ y -> x `Set.member` (fvt (Map.findWithDefault (Var y) y sub)))
                          (x `Set.delete` fv p)
              then variant x (fv (subst (x `Map.delete` sub) p))
              else x
    in  quant x' (subst (Map.insert x (Var x') sub) p)


{-  Prenex Normal Form  -}


simplify' :: Formula -> Formula
simplify' fm@(Forall x p) = if x `Set.member` fv p then fm else p
simplify' fm@(Exists x p) = if x `Set.member` fv p then fm else p
simplify' fm = p'simplify' fm


simplify :: Formula -> Formula
simplify (Not p) = simplify' (Not (simplify p))
simplify (And p q) = simplify' (And (simplify p) (simplify q))
simplify (Or p q) = simplify' (Or (simplify p) (simplify q))
simplify (Impl p q) = simplify' (Impl (simplify p) (simplify q))
simplify (Eq p q) = simplify' (Eq (simplify p) (simplify q))
simplify (Forall x p) = simplify' (Forall x (simplify p))
simplify (Exists x p) = simplify' (Exists x (simplify p))
simplify fm = fm


nnf :: Formula -> Formula
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q)
nnf (Impl p q) = Or (nnf (Not p)) (nnf q)
nnf (Eq p q) = (nnf p `And` nnf q) `Or` (nnf (Not p) `And` nnf (Not q))
nnf (Not (Not p)) = nnf p
nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
nnf (Not (Impl p q)) = And (nnf p) (nnf (Not q))
nnf (Not (Eq p q)) = (nnf p `And` nnf (Not q)) `Or` (nnf (Not p) `And` nnf q)
nnf (Forall x p) = Forall x (nnf p)
nnf (Exists x p) = Exists x (nnf p)
nnf (Not (Forall x p)) = Exists x (nnf (Not p))
nnf (Not (Exists x p)) = Forall x (nnf (Not p))
nnf fm = fm


pull'quants :: Formula -> Formula
pull'quants fm@(And (Forall x p) (Forall y q))
  = pull'q (True, True) fm Forall And x y p q
pull'quants fm@(Or (Exists x p) (Exists y q))
  = pull'q (True, True) fm Exists Or x y p q
pull'quants fm@(And (Forall x p) q)
  = pull'q (True, False) fm Forall And x x p q
pull'quants fm@(And p (Forall y q))
  = pull'q (False, True) fm Forall And y y p q
pull'quants fm@(Or (Forall x p) q)
  = pull'q (True, False) fm Forall Or x x p q
pull'quants fm@(Or p (Forall y q))
  = pull'q (False, True) fm Forall Or y y p q
pull'quants fm@(And (Exists x p) q)
  = pull'q (True, False) fm Exists And x x p q
pull'quants fm@(And p (Exists y q))
  = pull'q (False, True) fm Exists And y y p q
pull'quants fm@(Or (Exists x p) q)
  = pull'q (True, False) fm Exists Or x x p q
pull'quants fm@(Or p (Exists y q))
  = pull'q (False, True) fm Exists Or y y p q
pull'quants fm = fm


pull'q :: (Bool, Bool) -> Formula -> (String -> Formula -> Formula) -> (Formula -> Formula -> Formula) -> String -> String -> Formula -> Formula -> Formula
pull'q (l, r) fm quant op x y p q
  = let z = variant x (fv fm)
        p' = if l then subst (Map.singleton x (Var z)) p else p
        q' = if r then subst (Map.singleton y (Var z)) q else q
    in  quant z (pull'quants (op p' q'))


prenex :: Formula -> Formula
prenex (Forall x p) = Forall x (prenex p)
prenex (Exists x p) = Exists x (prenex p)
prenex (And p q) = pull'quants (And (prenex p) (prenex q))
prenex (Or p q) = pull'quants (Or (prenex p) (prenex q))
prenex fm = fm


pnf :: Formula -> Formula
pnf = prenex . nnf . simplify


{-  Skolemisation   -}


funcs :: Term -> Set.Set (String, Int)
funcs (Var _) = Set.empty
funcs (Fn n terms) = foldl' (\ s t -> s `Set.union` funcs t) (Set.singleton (n, length terms)) terms


functions :: Formula -> Set.Set (String, Int)
functions fm = atom'union (\ (Rel _ terms) -> foldl' (\ s t -> s `Set.union` funcs t) Set.empty terms) fm


skolem :: Formula -> Set.Set String -> (Formula, Set.Set String)
skolem fm@(Exists y p) fns
  = let xs  = Set.toList $! fv fm
        f   = variant y {- (if List.null xs then "c_" ++ y else "f_" ++ y) -} fns
        f'  = if List.null xs then 'ᶜ' : f else 'ᶠ' : f
        fx  = Fn f' (map Var xs)
    in  skolem (subst (Map.singleton y fx) p) (f `Set.insert` fns)
skolem fm@(Forall x p) fns
  = let (p', fns') = skolem p fns in (Forall x p', fns')
skolem fm@(And p q) fns
  = skolem' (\ (p, q) -> And p q) (p, q) fns
skolem fm@(Or p q) fns
  = skolem' (\ (p, q) -> Or p q) (p, q) fns
skolem fm fns
  = (fm, fns)


skolem' :: ((Formula, Formula) -> Formula) -> (Formula, Formula) -> Set.Set String -> (Formula, Set.Set String)
skolem' cons (p, q) fns
  = let (p', fns') = skolem p fns
        (q', fns'') = skolem q fns'
    in  (cons (p', q'), fns'')


a'skolemize :: Formula -> Formula
a'skolemize fm = fst $! skolem (nnf $! simplify fm) (Set.map fst (functions fm))


{-  NOTE: The following function clearly does not do "deep specialization".
          
          I think it is expected that it's called only on a formula after all the quantifiers have been pulled out.
          Or possibly after transforming it to Prenex Normal Form.    -}
specialize :: Formula -> Formula
specialize (Forall x p) = specialize p
specialize fm = fm


--  The definitions of `skolemize` from the book does specialization as well.
--  That is not something I want to have.
-- skolemize :: Formula -> Formula
-- skolemize fm = specialize $! pnf $! a'skolemize fm


skolemise :: Formula -> Formula
skolemise = pnf . a'skolemize


{-  Unification   -}


is'triv :: Map.Map String Term -> String -> Term -> Bool
is'triv env x (Var y)
  = y == x || y `Map.member` env && is'triv env x term
  where Just term = y `Map.lookup` env  -- NOTE: This pattern match is non-exhaustive but here, it's safe.

is'triv env x (Fn _ terms)
  = List.any (is'triv env x) terms


occurs'fails :: Map.Map String Term -> String -> Term -> Bool
occurs'fails env x (Var y)
  = y `Map.member` env && occurs'fails env x term
  where Just term = y `Map.lookup` env  -- NOTE: This pattern match is non-exhaustive but here, it's safe.
occurs'fails env x (Fn _ terms)
  = List.any (is'triv env x) terms


unify :: Map.Map String Term -> [(Term, Term)] -> Maybe (Map.Map String Term)
unify env [] = Just env

unify env ((fn@(Fn f f'args), gn@(Fn g g'args)) : eqs)
  = if f == g && length f'args == length g'args
    then unify env (zip f'args g'args ++ eqs)
    else Nothing -- error $! "impossible unification of " ++ show fn ++ " ≡ " ++ show gn

unify env ((Var x, t) : eqs)
  = if x `Map.member` env
    then unify env ((term, t) : eqs)
    else  if occurs'fails env x t
          then Nothing
          else unify (if is'triv env x t then env else Map.insert x t env) eqs
  where Just term = x `Map.lookup` env  -- NOTE: This pattern match is non-exhaustive but here, it's safe.

unify env ((t, Var x) : eqs)
  = unify env ((Var x, t) : eqs)


solve :: Map.Map String Term -> Map.Map String Term
solve env
  = let env' = Map.map (t'subst env) env
    in  if env' == env then env else solve env'


{-  QUESTION: Why don't we need to handle tautology as well?  -}
unify'literals :: Map.Map String Term -> (Formula, Formula) -> Maybe (Map.Map String Term)
unify'literals env (Atom (Rel p1 a1), Atom (Rel p2 a2))
  = unify env [(Fn p1 a1, Fn p2 a2)]
unify'literals env (Not p, Not q)
  = unify'literals env (p, q)
unify'literals env (S.False, S.False)
  = Just $! env
unify'literals _ _ = Nothing


unify'complements :: Map.Map String Term -> (Formula, Formula) -> Maybe (Map.Map String Term)
unify'complements env (p, q) = unify'literals env (p, negate q)


mgu :: [Formula] -> Map.Map String Term -> Maybe (Map.Map String Term)
mgu (a : b : rest) env
  = case unify'literals env (a, b) of
      Nothing -> Nothing
      Just env' -> mgu (b : rest) env'
mgu _ env = Just $! solve env


unifiable :: Formula -> Formula -> Bool
unifiable p q = Maybe.isJust $! unify'literals Map.empty (p, q)


rename :: String -> [Formula] -> [Formula]
rename prefix clauses
  = let fvs = Set.toList $! fv (list'disj clauses)
        vvs = List.map (\ s -> Var $! prefix ++ s) fvs
    in  map (subst (Map.fromList $! zip fvs vvs)) clauses


resolvents :: [Formula] -> [Formula] -> Formula -> Set.Set [Formula] -> Set.Set [Formula]
{-  p     is a particular Literal in cl1
    cl1   is some clause
    cl2   is some clause
    acc   is a List of Clauses              -}
resolvents cl1 cl2 p acc
  = let ps2 = Set.fromList $! filter (unifiable (negate p)) cl2
    {-  ps2   are all the literals in cl2 that would unify with ¬p    -}
    in  if List.null ps2
        then acc
        else  let ps1 :: Set.Set Formula  -- a Clause
                  ps1 = Set.fromList $! filter (\ q -> (q /= p) && p `unifiable` q) cl1
              {-  ps1   are all the literals in cl1 (excluding p) that could unify with p   -}

                  all'subsets'of'ps1 :: Set.Set (Set.Set Formula)
                  all'subsets'of'ps1 = (Set.map (Set.insert p) $! all'subsets ps1)

                  all'non'empty'subsets'of'ps2 :: Set.Set (Set.Set Formula)
                  all'non'empty'subsets'of'ps2 = all'non'empty'subsets ps2
                  
                  pairs :: [(Set.Set Formula, Set.Set Formula)]
                  pairs = Set.toList $! Set.cartesianProduct
                                          all'subsets'of'ps1
                                          all'non'empty'subsets'of'ps2
              
              in  foldl' (\ sof (s1, s2) -> case mgu (Set.toList (s1 `Set.union` Set.map negate s2)) Map.empty of
                                              Nothing -> sof
                                              Just sub -> 
                                                let resolvent = Set.map (subst sub) ((Set.fromList cl1 `Set.difference` s1) `Set.union` (Set.fromList cl2 `Set.difference` s2))
                                                in  Set.toList resolvent `Set.insert` sof
                ) acc pairs

  where all'subsets = Set.powerSet
        all'non'empty'subsets = Set.filter (not . Set.null) . Set.powerSet


resolve'clauses :: [Formula] -> [Formula] -> Set.Set [Formula]
resolve'clauses cls1 cls2
  = let cls1' = rename "x" cls1
        cls2' = rename "y" cls2
    in  foldl' (\ ress cls -> resolvents cls1' cls2' cls ress) Set.empty cls1'


{-  SUBSUMPTION  -}


term'match :: Map.Map String Term -> [(Term, Term)] -> Maybe (Map.Map String Term)
term'match env [] = Just env
term'match env ((Fn f fa, Fn g ga) : oth)
  | f == g && length ga == length ga = term'match env (zip fa ga ++ oth)
term'match env ((Var x, t) : oth)
  = if not (x `Map.member` env)
    then term'match (Map.insert x t env) oth
    else Nothing
term'match _ _ = Nothing


match'literals :: Map.Map String Term -> (Formula, Formula) -> Maybe (Map.Map String Term)
match'literals env (Atom (Rel p a1), Atom (Rel q a2))
  = term'match env [(Fn p a1, Fn q a2)]
match'literals env (Not (Atom (Rel p a1)), Not (Atom (Rel q a2)))
  = term'match env [(Fn p a1, Fn q a2)]
match'literals _ _ = Nothing


subsumes'clause :: [Formula] -> [Formula] -> Bool
subsumes'clause cl1 cl2 = subsume Map.empty cl1
  where subsume :: Map.Map String Term -> [Formula] -> Bool
        subsume _ [] = True
        subsume env (l1 : clt)
          = List.any (\ l2 -> case match'literals env (l1, l2) of
                                Nothing -> False
                                Just env -> subsume env clt) cl2


replace :: [Formula] -> [[Formula]] -> [[Formula]]
replace cl [] = [cl]
replace cl (c : cls)
  = if subsumes'clause cl c
    then cl : cls
    else c : replace cl cls


incorporate :: [Formula] -> [Formula] -> [[Formula]] -> [[Formula]]
incorporate gcl cl unused
  = if trivial cl || List.any (\ c -> subsumes'clause c cl) (gcl : unused)
    then unused
    else replace cl unused




-- replace' :: [Formula] -> Set.Set [Formula] -> Set.Set [Formula]
-- replace' cl clauses | Set.null clauses = Set.singleton cl
-- replace' cl clauses
--   = let c   = Set.elemAt 0 clauses
--         cls = Set.deleteAt 0 clauses
--     in  if subsumes'clause cl c
--         then cl `Set.insert` cls
--         else c `Set.insert` replace' cl cls


-- incorporate' :: [Formula] -> [Formula] -> Set.Set [Formula] -> Set.Set [Formula]
-- incorporate' gcl cl unused
--   = if trivial cl || List.any (\ c -> subsumes'clause c cl) (gcl `Set.insert` unused)
--     then unused
--     else replace' cl unused


-- res'loop' :: (Set.Set [Formula], Set.Set [Formula]) -> Bool
-- res'loop' (used, unused)
--   | Set.null unused = False
--   | otherwise = let cl = Set.elemAt 0 unused
--                     cls = Set.deleteAt 0 unused

--                     used' = cl `Set.insert` used
--                     resolvents = Set.map (resolve'clauses cl) used'
--                     news = Set.foldl' Set.union Set.empty resolvents
--                     news' = trace ("#used= " ++ show (length used) ++ " | size of cl= " ++ show (length cl) ++ "\nsize of unused= " ++ show (Set.size unused) {- ++ "\ncl= " ++ show cl -}) news
--                     fn acc new = incorporate' cl new acc

--                     new'unused = Set.foldl' fn cls news'
--                     -- new'unused = cls `Set.union` news'

--                     how'many'actually = (Set.size news') - ((Set.size new'unused) - (Set.size cls))
--                     new'un' = trace ("news count= " ++ show (Set.size news') ++ " | cls count= " ++ show (Set.size cls) ++ " | new'un' count= " ++ show (Set.size new'unused) ++ "\n || how many actually NOT ADDED= " ++ show (how'many'actually) ) new'unused
--                 in  ([] `Set.member` news') || res'loop' (used', new'un')


res'loop :: ([[Formula]], [[Formula]]) -> Bool
res'loop (_, []) = False -- the book raises an error, I don't see why
res'loop (used, unused@(cl : cls))
  = let used' = Set.toList $! cl `Set.insert` Set.fromList used
        resolvents = map (resolve'clauses cl) used'
        news = foldl' Set.union Set.empty resolvents
        fn a b = incorporate cl b a

        new'unused = List.foldl' fn cls (Set.toList news)
    -- in  ([] `List.elem` news) || res'loop (used', new'unused)
    in  ([] `List.elem` news) || res'loop (used', cls ++ Set.toList news)


res'loop'' :: ([[Formula]], [[Formula]]) -> Maybe [Formula]
res'loop'' (_, []) = Nothing
res'loop'' (used, unused@(cl : cls))
  = let used' = Set.toList $! cl `Set.insert` Set.fromList used
        resolvents = map (resolve'clauses cl) used'
        news = Set.toList $! foldl' Set.union Set.empty resolvents
        fn a b = incorporate cl b a

        -- new'unused = List.foldl' fn cls (Set.toList news)
    -- in  ([] `List.elem` news) || res'loop'' (used', new'unused)
    in  case contains'contradiction news of
          Nothing -> res'loop'' (used', cls ++ news)
          Just answers -> Just answers


contains'contradiction :: [[Formula]] -> Maybe [Formula]
contains'contradiction = List.find (\ formulae -> List.all is'answer formulae && all'unique (List.map (\ (Atom (Rel "_Answer_" [_, Fn x _])) -> x) formulae))


all'unique :: [String] -> Bool
all'unique names = List.length names == Set.size (Set.fromList names)


is'answer :: Formula -> Bool
is'answer (Atom (Rel "_Answer_" [_, _])) = True
is'answer _ = False


pure'resolution' :: Formula -> Bool
-- pure'resolution' fm = res'loop' (Set.empty, Set.fromList . simp'cnf . specialize . skolemise $! fm)
pure'resolution' fm = res'loop ([], simp'cnf . specialize . skolemise $! fm)


pure'resolution'' :: Formula -> Maybe [Formula]
pure'resolution'' fm = res'loop'' ([], simp'cnf . specialize . skolemise $! fm)


resolution'' :: [Formula] -> Formula -> Maybe [(String, Term)]
resolution'' assumptions fm@(Exists x p)
  = -- find all the existentially quantified variables at the top level
    let all'exs = all'existentials fm
        neg'fm  = nnf (negate fm)
        -- I can look at what names the algorithm choses for all those (now) foralls
        -- I create all those Answer(..) literals with those same names and make one large disjunction
        (all'unis, p)  = all'universals neg'fm
        answers   = map (\ (u, e) -> Atom (Rel "_Answer_" [Var u, Fn e []])) $! zip all'unis all'exs
        disjunction = List.foldr Forall (list'disj (p : answers)) all'unis
        -- disjunction = list'disj (neg'fm : answers)
        full'fm   = list'conj (disjunction : assumptions)
    in  case pure'resolution'' full'fm of
          Nothing -> Nothing
          Just formulae ->
            Just $! map (\ (Atom (Rel "_Answer_" [term, Fn name []])) -> (name, term)) formulae


resolution :: [Formula] -> Formula -> Bool  
resolution assumptions conclusion
  = let neg'conclusion = negate conclusion
        fm = list'conj (neg'conclusion : assumptions)
    in  pure'resolution' fm


all'existentials :: Formula -> [String]
all'existentials (Exists x p) = x : all'existentials p
all'existentials _ = []


all'universals :: Formula -> ([String], Formula)
all'universals (Forall x p) = let (exis, q) = all'universals p in (x : exis, q)
all'universals p = ([], p)


neg'norm'form :: Formula -> Formula
neg'norm'form = nnf . simplify


pren'norm'form :: Formula -> Formula
pren'norm'form = pnf . simplify


skol'norm'form :: Formula -> Formula
skol'norm'form = skolemise


--  NOTE: I am skipping `pnf` there, I still think it should work, but it needs some testing.
con'norm'form :: Formula -> Formula
con'norm'form = specialize . prenex . a'skolemize -- roughly: skolem . nnf . simplify


features'exists :: Formula -> Bool
features'exists S.True = False
features'exists S.False = False
features'exists (Atom _) = False
features'exists (Not p) = features'exists p
features'exists (And p q) = features'exists p || features'exists q
features'exists (Or p q) = features'exists p || features'exists q
features'exists (Impl p q) = features'exists p || features'exists q
features'exists (Eq p q) = features'exists p || features'exists q
features'exists (Forall _ p) = features'exists p
features'exists (Exists _ _) = True
