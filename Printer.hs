module Printer
(
    show,
    pShow
)
where

import           Data.Maybe
import           Control.Applicative
import           Prelude   hiding (showList)
import           Text.Show hiding (showList)
import qualified Numeric  as N
import qualified Data.Set as S

import Lambda

{-
    The pretty-printing function detects whether it's printing a Church-numeral, a Church-pair,
    or a list (represented by nested pairs), and displays them with special syntax.
-}

pShow :: Term -> String
pShow t = pShowTerm False t ""

pShowTerm :: Bool -> Term -> ShowS
pShowTerm b t = fromMaybe (showParen b (go t)) (showInt t <|> showList t <|> showPair t)
  where
    go :: Term -> ShowS
    go (Var x)   = showString x
    go (Abs x y) = showString ('λ' : x)  . showChar '.' . pShowTerm False y
    go (App x y) = pShowTerm (isAbs x) x . showChar ' ' . pShowTerm (not (isVar y)) y

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _         = False

{-
    The 'toX' functions attempt to convert Church-numerals, Church-pairs, and lists to the Haskell
    equivalents, then the 'showX' functions attempt to show them, failing if the conversion failed.
-}

toInt :: Term -> Maybe Int
toInt (Abs f (Abs x t)) | f /= x = go 0 t
  where
    go n (Var a)         | x == a = pure n
    go n (App (Var a) b) | f == a = go (n+1) b
    go _ _                        = Nothing
toInt _ = Nothing

showInt :: Term -> Maybe ShowS
showInt = fmap N.showInt . toInt

toPair :: Term -> Maybe (Term, Term)
toPair (Abs x (App (App (Var y) u) v)) | x == y && x `notElem` freeVars u `S.union` freeVars v = pure (u,v)
toPair _                                                                                       = Nothing

showPair :: Term -> Maybe ShowS
showPair = fmap (\(x,y) -> showChar '<'
                         . pShowTerm False x
                         . showChar ','
                         . pShowTerm False y
                         . showChar '>') . toPair

toList :: Term -> Maybe [Term]
toList (Abs _ (Abs x (Abs y (Var z)))) | x == z && y /= z = pure []
toList t                                                  = do (x,y) <- toPair t
                                                               (x:) <$> toList y
showList :: Term -> Maybe ShowS
showList = fmap (showListWith (pShowTerm False)) . toList
