module Lambda
(
    Term(..),
    Name,
    freeVars,
    descend,
    descendA,
    normOrder,
    applOrder,
    normalise,
    reductions,
    normEval,
    applEval
)
where

import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M

{-
    A module implementing the core Term type, and functions for
    reducing, normalising, or otherwise working with λ-terms.
-}

type Name = String
data Term = Var Name
          | Abs Name Term
          | App Term Term

freeVars :: Term -> Set Name
freeVars (Var v)   = S.singleton v
freeVars (Abs v t) = S.delete v (freeVars t)
freeVars (App x y) = freeVars x `S.union` freeVars y

allVars :: Term -> Set Name
allVars (Var v)   = S.singleton v
allVars (Abs v t) = S.insert v (allVars t)
allVars (App x y) = allVars x `S.union` allVars y

{-
    Two λ-terms are α-equivalent if the bound variables
    of one can be renamed to obtain the other.
-}

instance Eq Term where
    (==) = eqTerm 0 M.empty M.empty


eqTerm :: Int -> Map Name Int -> Map Name Int -> Term -> Term -> Bool
eqTerm _ m1 m2 (Var x) (Var y) = let v1 = M.lookup x m1
                                 in case M.lookup y m2 of
                                      Just  n -> v1 == Just n
                                      Nothing -> isNothing v1 && x == y
eqTerm n m1 m2 (Abs x y) (Abs a b) = eqTerm (n+1) (M.insert x n m1) (M.insert a n m2) y b
eqTerm n m1 m2 (App x y) (App a b) = eqTerm n m1 m2 x a && eqTerm n m1 m2 y b
eqTerm _ _ _ _ _ = False

{-
    Abstracts a common pattern when working with λ-terms, allowing functions
    to pass their recursive calls to 'decend' in catch-all patterns.
-}

descend  :: (Term -> Term) -> Term -> Term
descend _ (Var x)   = Var x
descend f (Abs x y) = Abs x (f y)
descend f (App x y) = App (f x) (f y)

descendA :: Applicative f => (Term -> f Term) -> Term -> f Term
descendA _ (Var x)   = pure (Var x)
descendA f (Abs x y) = Abs x <$> f y
descendA f (App x y) = App <$> f x <*> f y

{-
    Functions for performing substitution and dealing with name capture.
-}

substitute :: Name -> Term -> Term -> Term
substitute a t (Var x)   = if x == a then t       else Var x
substitute a t (Abs x y) = if x == a then Abs x y else Abs x (substitute a t y)
substitute a t x         = descend (substitute a t) x

rename :: Name -> Name -> Term -> Term
rename a = substitute a . Var

betaReduce :: Term -> Term
betaReduce t = let App (Abs x y) z = captureAvoid t
               in  substitute x z y

captureAvoid :: Term -> Term
captureAvoid (App (Abs x y) z) = App (captureAvoid' (freeVars z) (Abs x y)) z

captureAvoid' :: Set Name -> Term -> Term
captureAvoid' ns (Abs x y) | x `elem` ns = Abs x' (captureAvoid' ns y')
                           | otherwise   = Abs x  (captureAvoid' ns y)
                           where x'    = freshName (allVars y) x
                                 y'    = rename x x' y
captureAvoid' ns t         = descend (captureAvoid' ns) t

freshName :: Set Name -> Name -> Name
freshName ns n | n' `elem` ns = freshName ns n'
               | otherwise    = n'
               where n' = nextName n

nextName :: Name -> Name
nextName []       = "a"
nextName ('z':xs) = 'a' : nextName xs
nextName ( x :xs) = succ x : xs

{-
    Functions for performing single reduction steps, for the normal order
    and applicative order reduction strategies. They can be iterated to
    fully-normalise terms and obtain a list of all intermediate terms.
-}

isNF :: Term -> Bool
isNF (App (Abs _ _) _) = False
isNF (Var _)           = True
isNF (Abs _ y)         = isNF y
isNF (App x y)         = isNF x && isNF y

normOrder :: Term -> Term
normOrder r@(App (Abs _ _) _) = betaReduce r
normOrder (App x y)           | isNF x    = App x (normOrder y)
                              | otherwise = App (normOrder x) y
normOrder t                   = descend normOrder t

applOrder :: Term -> Term
applOrder r@(App a@(Abs _ _) z) | isNF z   = betaReduce r
                                | otherwise = App a (applOrder z)
applOrder (App x y)             | isNF x   = App x (applOrder y)
                                | otherwise = App (applOrder x) y
applOrder t                     = descend applOrder t

normalise :: (Term -> Term) -> Term -> Term
normalise f = go
  where
    go t = if isNF t then t else go (f t)

reductions :: (Term -> Term) -> Term -> [Term]
reductions f = go
  where
    go t = t : if isNF t then [] else go (f t)

{-
    More efficient functions for fully-normalising terms. These don't
    need to traverse from the root of the term at each reduction step.
-}

normEval :: Term -> Term
normEval r@(App (Abs _ _) _)   = normEval $ betaReduce r
normEval (App x y) | isNF x    = App x (normEval y)
                   | otherwise = normEval $ App (normOrder x) y
normEval t                     = descend normEval t

applEval :: Term -> Term
applEval (App a@(Abs _ _) z)   = let z' = applEval z in
                                 z' `seq` applEval (betaReduce $ App a z')
applEval (App x y) | isNF x    = App x (applEval y)
                   | otherwise = applEval $ App (applOrder x) y
applEval t                     = descend applEval t
