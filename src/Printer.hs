module Printer
(
    show,
    pShow
)
where

import           Prelude   hiding (showList)
import           Text.Show hiding (showList)
import           Data.Maybe
import qualified Numeric  as N
import           Control.Applicative

import Lambda

{-
    The pretty-printing function detects whether it's printing a Church-numeral,
    a Church-pair, or a Church-list and displays them with special syntax.
-}

pShow :: Term -> String
pShow t = pShowTerm False t ""

pShowTerm :: Bool -> Term -> ShowS
pShowTerm b t = fromMaybe (showParen b (go t)) (showInt t <|> showList t <|> showPair t)
  where
    go :: Term -> ShowS
    go (Var x)   = showString x
    go (Abs x y) = showString ('λ' : x)  . showChar '.' . pShowTerm False y
    go (App x y) = pShowTerm (isAbs x) x . showChar ' ' . pShowTerm (notVar y) y

notVar :: Term -> Bool
notVar (Var _) = False
notVar _       = True

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
    go  n (Var a)         | x == a = Just n
    go !n (App (Var a) b) | f == a = go (n+1) b
    go _ _                         = Nothing
toInt _ = Nothing

showInt :: Term -> Maybe ShowS
showInt = fmap N.showInt . toInt

toPair :: Term -> Maybe (Term, Term)
toPair (Abs x (App (App (Var y) u) v)) | x == y && x `notElem` freeVars u && x `notElem` freeVars v = Just (u,v)
toPair _                                                                                            = Nothing

showPair :: Term -> Maybe ShowS
showPair = fmap (\(x,y) -> showChar '<'
                         . pShowTerm False x
                         . showChar ','
                         . pShowTerm False y
                         . showChar '>') . toPair

toList :: Term -> Maybe [Term]
toList (Abs f (Abs x t)) | f /= x = go t
  where
    go :: Term -> Maybe [Term]
    go (Var a)                 | x == a                                                     = Just []
    go (App (App (Var a) u) b) | f == a && f `notElem` freeVars u && x `notElem` freeVars u = (u:) <$> go b
    go _                                                                                    = Nothing
toList _ = Nothing

showList :: Term -> Maybe ShowS
showList = fmap (showListWith (pShowTerm False)) . toList
