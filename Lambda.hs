module Lambda
(
    Term(..),
    freeVars,
    freshName,
    nextName,
    normOrder,
    applOrder,
    reductions,
    normEval,
    applEval,
)
where

import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.Foldable
import Data.Function
import Control.Monad.Trans.State.Strict

import Debug.Trace

{-
    A module implementing the core Term type, and functions for
    reducing, normalising, or otherwise working with λ-terms.
-}

data Term = Var String
          | Abs String Term
          | App Term Term

freeVars :: Term -> Set String
freeVars (Var v)   = S.singleton v
freeVars (Abs v t) = S.delete v (freeVars t)
freeVars (App x y) = freeVars x `S.union` freeVars y

closed :: Term -> Bool
closed = S.null . freeVars

{-
    Two λ-terms are α-equivalent if the bound variables
    of one can be renamed to obtain the other.
-}

instance Eq Term where
    (==) = eqTerm 0 M.empty M.empty


eqTerm :: Int -> Map String Int -> Map String Int -> Term -> Term -> Bool
eqTerm _ m1 m2 (Var x) (Var y) = let v1 = M.lookup x m1
                                 in case M.lookup y m2 of
                                      Just  n -> v1 == Just n
                                      Nothing -> isNothing v1 && x == y
eqTerm n m1 m2 (Abs x y) (Abs a b) = eqTerm (n+1) (M.insert x n m1) (M.insert a n m2) y b
eqTerm n m1 m2 (App x y) (App a b) = eqTerm n m1 m2 x a && eqTerm n m1 m2 y b
eqTerm _ _ _ _ _ = False

instance Show Term where
    showsPrec _ (Var x)   = showString x
    showsPrec p (Abs x y) = showParen (p > 1) (showString ('λ' : x) . showChar '.' . showsPrec 1 y)
    showsPrec p (App x y) = showParen (p > 2) (showsPrec 2 x        . showChar ' ' . showsPrec 3 y)

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
descendA f (Abs x y) = Abs       x <$> f y
descendA f (App x y) = App <$> f x <*> f y

{-
    Functions for performing substitution and dealing with name capture.
-}

beta :: String -> Term -> Term -> Term
beta x z = substitute (S.insert x (freeVars z)) x z

substitute :: Set String -> String -> Term -> Term -> Term
substitute fv a t (Var x)   = if x == a then t else Var x
substitute fv a t (App x y) = App (substitute fv a t x) (substitute fv a t y)
substitute fv a t (Abs x y) | a == x           = Abs x y
                            | S.notMember x fv = Abs x  (substitute fv a t y)
                            | otherwise        = let fn = freshName (fv `S.union` freeVars y)
                                                 in Abs fn (substitute fv a t (substitute S.empty x (Var fn) y))

freshName :: Set String -> String
freshName = nextName . maximumBy compareNames . S.insert ""
  where
    compareNames :: String -> String -> Ordering
    compareNames = compareLength <> compare
      where
        compareLength :: [a] -> [a] -> Ordering
        compareLength []     []     = EQ
        compareLength []     (_:_)  = LT
        compareLength (_:_)  []     = GT
        compareLength (_:xs) (_:ys) = compareLength xs ys

nextName :: String -> String
nextName = reverse . go . reverse
  where
    go ""       = "a"
    go ('z':xs) = 'a' : nextName xs
    go ( x :xs) = succ x : xs

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
normOrder (App (Abs x y) z) = beta x z y
normOrder (App x y)         | isNF x    = App x (normOrder y)
                            | otherwise = App (normOrder x) y
normOrder t                 = descend normOrder t

applOrder :: Term -> Term
applOrder (App (Abs x y) z) | isNF z    = beta x z y
                            | otherwise = App (Abs x y) (applOrder z)
applOrder (App x y)         | isNF x    = App x (applOrder y)
                            | otherwise = App (applOrder x) y
applOrder t                 = descend applOrder t

reductions :: (Term -> Term) -> Term -> [Term]
reductions f = go
  where
    go t = t : if isNF t then [] else go (f t)

{-
    More efficient functions for fully-normalising terms. These don't
    need to traverse from the root of the term at each reduction step.
-}

normEval :: Term -> Term
normEval (App (Abs x y) z)     = normEval (beta x z y)
normEval (App x y) | isNF x    = App x (normEval y)
                   | otherwise = normEval (App (normOrder x) y)
normEval t                     = descend normEval t

applEval :: Term -> Term
applEval (App (Abs x y) z)     = let z' = applEval z in
                                 z' `seq` applEval (beta x z y)
applEval (App x y) | isNF x    = App x (applEval y)
                   | otherwise = applEval (App (applOrder x) y)
applEval t                     = descend applEval t
