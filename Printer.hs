module Printer
(
    show,
    pShow
)
where

import Prelude hiding (showList)

import Numeric
import Text.Show hiding (showList)
import Data.Maybe
import Control.Applicative
import qualified Data.Set as S

import Lambda

{-------------------------------------------------------------------------------

Printing function for lambda-terms. Prints terms with their standard inline
syntax. Uses difference lists with ShowS to avoid large left-associations of
(++). Decides whether to surround subterms with parentheses by passing booleans
to showParen.

-}

instance Show Term where
    showsPrec _ (Var x) = showString x
    showsPrec p (Abs x y) = showParen (p > 4) $ showString ('λ' : x) . showChar '.' . showsPrec 4 y
    showsPrec p (App x y) = showParen (p > 5) $ showsPrec 5 x . showChar ' ' . showsPrec 6 y

{-------------------------------------------------------------------------------

Pretty printing function for lambda-terms. Uses difference lists, just like
'show'.

pShowTerm decides which pretty printing rule to use with Maybe's (<|>) choice
operator, defaulting to the lambda-term's standard inline syntax with
pShowTerm', and deciding whether to surround subterms with parentheses using
its Bool argument.

-}

pShow :: Term -> String
pShow t = pShowTerm False t ""

pShowTerm :: Bool -> Term -> ShowS
pShowTerm b t = fromJust $ showNumeral  t <|>
                           showList t     <|>
                           showPair t     <|>
                           Just (showParen b $ pShowTerm' t)


pShowTerm' :: Term -> ShowS
pShowTerm' (Var x)   = showString x
pShowTerm' (Abs x y) = showString ('\x03bb' : x)
                     . showChar '.'
                     . pShowTerm False y
pShowTerm' (App x y) = pShowTerm (isAbs x) x
                     . showChar ' '
                     . pShowTerm (isAbs y || isApp y) y

{-------------------------------------------------------------------------------

Pretty printing functions for data types. Each runs a function (returning in
Maybe) to convert a lambda-term to the respective data type, and then 'fmap's a
showing function over the result.

showRef looks up a reference name from a lambda-term in the environment (each
element already swapped by pShow)

-}

showNumeral :: Term -> Maybe ShowS
showNumeral = fmap showInt . toInt

showList :: Term -> Maybe ShowS
showList = fmap (showListWith $ pShowTerm False) . toList

showPair :: Term -> Maybe ShowS
showPair = fmap (\(x,y) -> showChar '<'
                         . pShowTerm False x
                         . showChar ','
                         . pShowTerm False y
                         . showChar '>') . toPair

{-------------------------------------------------------------------------------

Functions to convert from lambda-terms to Haskell data types, so they may then
be printed. This is simply done by pattern matching, with recursion in the toInt
and toList cases.

-}

toInt :: Term -> Maybe Int
toInt (Abs f (Abs x t)) | f /= x = toInt' f x 0 t
toInt _ = Nothing

toInt' :: Name -> Name -> Int -> Term -> Maybe Int
toInt' f x n (Var a)         | x == a = Just n
toInt' f x n (App (Var a) b) | f == a = toInt' f x (n+1) b
toInt' _ _ _ _ = Nothing

toPair :: Term -> Maybe (Term, Term)
toPair (Abs x (App (App (Var y) u) v)) | x == y && x `notElem` freeVars u `S.union` freeVars v = return (u,v)
toPair _ = Nothing

toList :: Term -> Maybe [Term]
toList (Abs _ (Abs x (Abs y (Var z)))) | x == z && y /= z = return []
toList t = do (x,y) <- toPair t
              xs    <- toList y
              return (x:xs)

{-------------------------------------------------------------------------------

Predicates used for computing arguments to showParen.

-}

isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _         = False

isApp :: Term -> Bool
isApp (App _ _) = True
isApp _         = False
