module Lambda
(
    Term(..),
    Name,
    freeVars,
    descend,
    descendM,
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

{-------------------------------------------------------------------------------

A lambda-term is a variable, an abstraction, or an application. A name is just a
string.

-}

data Term = Var Name
          | Abs Name Term
          | App Term Term
type Name = String

{-------------------------------------------------------------------------------

This Eq instance accounts for alpha-equivalence where Haskell's derived instance
wouldn't.

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

{-------------------------------------------------------------------------------

Abstracts a common pattern when programming with terms, allowing functions to
pass their recursive calls to descend in catch-all patterns. Monadic functions
can pass their recursive calls to descendM.

-}

descend  :: (Term -> Term) -> Term -> Term
descend f (Var x)   = Var x
descend f (Abs x y) = Abs x (f y)
descend f (App x y) = App (f x) (f y)

descendM :: Monad m => (Term -> m Term) -> Term -> m Term
descendM f (Var x)   = return $ Var x
descendM f (Abs x y) = do { y' <- f y ; return $ Abs x y'}
descendM f (App x y) = do { x' <- f x ; y' <- f y ; return $ App x' y' }

{-------------------------------------------------------------------------------

Substitutes all variables with a particular name with a term for another term.
Accounts for abstraction scoping rules, not substituting inside an abstraction
which abstracts the same name.

-}

substitute :: Name -> Term -> Term -> Term
substitute a t (Var x)   = if x == a then t       else Var x
substitute a t (Abs x y) = if x == a then Abs x y else Abs x (substitute a t y)
substitute a t x         = descend (substitute a t) x

{-------------------------------------------------------------------------------

A special case of substitution which substitutes all variables with a particular
name for a variable of another name.

-}

rename :: Name -> Name -> Term -> Term
rename a = substitute a . Var

{-------------------------------------------------------------------------------

Using substitute, performs beta-reduction on a reducible expression after
alpha-converting it to a form insusceptible to name capture.

-}

betaReduce :: Term -> Term
betaReduce t = let App (Abs x y) z = captureAvoid t
               in  substitute x z y

{-------------------------------------------------------------------------------

Performs alpha-conversion on a redex to obtain a form insusceptible to name
capture. Checks for abstractions which abstract a name in the set of free
variables of the argument to the redex, renaming them.

-}

captureAvoid :: Term -> Term
captureAvoid (App (Abs x y) z) = App (captureAvoid' (freeVars z) (Abs x y)) z

captureAvoid' :: Set Name -> Term -> Term
captureAvoid' ns (Abs x y) | x `elem` ns = Abs x' (captureAvoid' ns y')
                           | otherwise   = Abs x  (captureAvoid' ns y)
                           where x'    = freshName (allVars y) x
                                 y'    = rename x x' y
captureAvoid' ns t         = descend (captureAvoid' ns) t

{-------------------------------------------------------------------------------

Finds a name not in the argument list by repeatedly applying nextName until one
is found.

-}

freshName :: Set Name -> Name -> Name
freshName ns n | n' `elem` ns = freshName ns n'
               | otherwise    = n'
               where n' = nextName n

{-------------------------------------------------------------------------------

Generates a new name from an old one. Names must only consist of lower-case
characters. Cycles through 'a'..'z' in the head of the list, and propagates an
overflow when 'z' is reached, replacing the 'z' with 'a', and recursing on the
tail.

-}

nextName :: Name -> Name
nextName []       = "a"
nextName ('z':xs) = 'a' : nextName xs
nextName ( x :xs) = succ x : xs

{-------------------------------------------------------------------------------

Generates a list representing the set of free variables of a term.

-}

freeVars :: Term -> Set Name
freeVars (Var v)   = S.singleton v
freeVars (Abs v t) = S.delete v (freeVars t)
freeVars (App x y) = freeVars x `S.union` freeVars y

allVars :: Term -> Set Name
allVars (Var v)   = S.singleton v
allVars (Abs v t) = S.insert v (allVars t)
allVars (App x y) = allVars x `S.union` allVars y

{-------------------------------------------------------------------------------

A function to perform a single step of beta-reduction with the normal order
reduction strategy.

-}

normOrder :: Term -> Term
normOrder r@(App (Abs _ _) _) = betaReduce r
normOrder (App x y)           | isNF x    = App x (normOrder y)
                              | otherwise = App (normOrder x) y
normOrder t                   = descend normOrder t

{-------------------------------------------------------------------------------

A function to perform a single step of beta-reduction with the applicative
order reduction strategy.

-}

applOrder :: Term -> Term
applOrder r@(App a@(Abs _ _) z) | isNF z   = betaReduce r
                                | otherwise = App a (applOrder z)
applOrder (App x y)             | isNF x   = App x (applOrder y)
                                | otherwise = App (applOrder x) y
applOrder t                     = descend applOrder t

{-------------------------------------------------------------------------------

The normalise function will evaluate a term to its normal form (if it has one),
while reductions will return a complete list of every step along the way.

-}

normalise :: (Term -> Term) -> Term -> Term
normalise f = go
  where
    go t = if isNF t then t else go (f t)

reductions :: (Term -> Term) -> Term -> [Term]
reductions f = go
  where
    go t = t : if isNF t then [] else go (f t)

{-------------------------------------------------------------------------------

A more efficient function to fully evaluate a term to its normal form with
normal order reduction.

-}

normEval :: Term -> Term
normEval r@(App (Abs _ _) _)   = normEval $ betaReduce r
normEval (App x y) | isNF x    = App x (normEval y)
                   | otherwise = normEval $ App (normOrder x) y
normEval t         = descend normEval t

{-------------------------------------------------------------------------------

A more efficient function to fully evaluate a term to its normal form with
applicative order reduction.

-}

applEval :: Term -> Term
applEval (App a@(Abs _ _) z)   = let z' = applEval z in
                                 z' `seq` applEval (betaReduce $ App a z')
applEval (App x y) | isNF x   = App x (applEval y)
                   | otherwise = applEval $ App (applOrder x) y
applEval t         = descend applEval t

{-------------------------------------------------------------------------------

Predicate to decide whether a term is a normal form.

-}

isNF :: Term -> Bool
isNF (App (Abs _ _) _) = False
isNF (Var _)           = True
isNF (Abs _ y)         = isNF y
isNF (App x y)         = isNF x && isNF y
