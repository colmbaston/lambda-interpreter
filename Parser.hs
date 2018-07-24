module Parser where

import qualified Data.Map as M
import           Data.Map (Map)

import Data.Char
import Data.Void
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char

import Lambda

-- TYPES

type Parser = Parsec Void String

data Input = Term Term
           | Reds Term
           | Let String Term
           | Script FilePath
           | Eval EvalStrat
           | PPrint
           | Help
           | Exit
           | Comment

data EvalStrat = Norm | Appl | Off

parseInput :: Map String Term -> String -> Maybe Input
parseInput env str = case parseMaybe inputParser str of
                       Nothing        -> Nothing
                       Just (Term t)  -> Term  <$> dereference env t
                       Just (Reds t)  -> Reds  <$> dereference env t
                       Just (Let n t) -> Let n <$> mfilter (null . freeVars) (dereference env t)
                       i              -> i

-- UTILS

insensitive :: String -> Parser String
insensitive = try . mapM (\c -> char (toUpper c) <|> char (toLower c) >> return c)

chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 p op = p >>= rest
  where
    rest x = try (do f <- op
                     b <- p
                     rest (f x b)) <|> pure x

trim :: Parser a -> Parser a
trim p = do space
            x <- p
            space
            pure x

parens :: Parser a -> Parser a
parens p = do char '('
              x <- trim p
              char ')'
              pure x

-- TERM PARSER

termParser :: Parser Term
termParser = trim absLevel

absLevel :: Parser Term
absLevel = appLevel <|> do char '\\' <|> char 'λ'
                           x <- trim (some (satisfy isLower))
                           char '.'
                           Abs x <$> termParser

appLevel :: Parser Term
appLevel = chainl1 atomLevel (space1 >> pure App)

atomLevel :: Parser Term
atomLevel = Var <$> some (satisfy isValid) <|> parens absLevel

isValid :: Char -> Bool
isValid c = c `notElem` "\\λ.()"
         && c >= '!'
         && c <= '}'

-- INPUT PARSER

inputParser :: Parser Input
inputParser = trim (try (char '~' >> commandParser)
                <|> commentParser
                <|> Term <$> termParser)

commandParser :: Parser Input
commandParser =  Let                          <$> (insensitive "let"        >> space1 >> ref) <*> (space1 >> string ":=" >> termParser)
             <|> Reds                         <$> (insensitive "reductions" >> space1 >> termParser)
             <|> Eval                         <$> (insensitive "eval"       >> space1 >> stratParser)
             <|> const PPrint                 <$>  insensitive "pprint"
             <|> Script                       <$> (insensitive "script"     >> space1 >> some anyChar)
             <|> const (Script "lib/Prelude") <$>  insensitive "prelude"
             <|> const Help                   <$>  insensitive "help"
             <|> const Exit                   <$>  insensitive "exit"

ref :: Parser String
ref = do x <- some (satisfy isValid)
         guard (isRefName x)
         pure x

isRefName :: String -> Bool
isRefName = not . (\c -> isLower c || isDigit c) . head

commentParser :: Parser Input
commentParser =  const Comment <$> (string "~~" >> many anyChar)
             <|> const Comment <$> eof

stratParser :: Parser EvalStrat
stratParser =  const Norm <$> insensitive "norm"
           <|> const Appl <$> insensitive "appl"
           <|> const Off  <$> insensitive "off"

-- DEREFERENCING IDENTIFIERS AND NUMERALS

dereference :: Map String Term -> Term -> Maybe Term
dereference env (Var x) | all isLower x = pure (Var x)
                        | all isDigit x = pure (toNumeral (read x))
                        | isRefName   x = M.lookup x env
                        | otherwise     = Nothing
dereference env t       = descendM (dereference env) t

toNumeral :: Int -> Term
toNumeral n = Abs "f" (Abs "x" (iterate (App (Var "f")) (Var "x") !! n))
