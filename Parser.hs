module Parser where

import           Data.Void
import           Data.Char
import qualified Data.Map as M
import           Data.Map (Map)
import           Control.Monad
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Lambda

{-
    A module for parsing interpreter input from strings using the Megaparsec
    library. As well as parsing raw λ-terms and interpreter commands, it
    needs to be able to dereference names for λ-terms that have previously
    been added to the interpreter environment via let commands.
-}

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

data EvalStrat = Norm
               | Appl
               | Off

parseInput :: Map String Term -> String -> Maybe Input
parseInput env str = case parseMaybe inputParser str of
                       Nothing        -> Nothing
                       Just (Term t)  -> Term  <$> dereference env t
                       Just (Reds t)  -> Reds  <$> dereference env t
                       Just (Let n t) -> Let n <$> mfilter (null . freeVars) (dereference env t)
                       i              -> i

{-
    Some useful parser combinators to augment the set of Megaparsec combinators.
-}

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

insensitive :: String -> Parser String
insensitive = try . mapM (\c -> char (toUpper c) <|> char (toLower c) >> return c)

{-
    The parser for λ-terms, loosely based on the following context-free grammar:

     -> ABS   ::= λ VAR . ABS
               |  APP

        APP   ::= APP [space] ATOM
               |  ATOM

        ATOM  ::= VAR
               |  NAT
               |  IDENT
               |  ( ABS )

        VAR   ::= [non-empty string of lower-case letters]
        NAT   ::= [non-empty string of decimal digits]
        IDENT ::= [non-empty string of valid characters, not beginning with a lower-case letter or a decimal digit]

    This is an unambiguous grammar that correctly handles the precedence and association
    rules for abstractions and applications. The NAT nonterminal allows for Church-numerals
    to be specified by their decimal representation, and the IDENT nonterminal allows λ-terms
    in the interpreter environment to be referred to by their identifier.
-}

termParser :: Parser Term
termParser = trim absLevel

absLevel :: Parser Term
absLevel = appLevel <|> do char '\\' <|> char 'λ'
                           x <- trim (some (satisfy isLower))
                           char '.'
                           Abs x <$> termParser

appLevel :: Parser Term
appLevel = chainl1 atomLevel (const App <$> space1)

atomLevel :: Parser Term
atomLevel = Var <$> some (satisfy isValid) <|> parens absLevel

isValid :: Char -> Bool
isValid c = c >= '!' && c <= '}' && c `notElem` "\\λ.()"

{-
    Parser for interpreter inputs. Commands are strings beginning with a single '~', comments
    contain only spaces or begin with two '~', and anything else is a λ-term.
-}

inputParser :: Parser Input
inputParser = trim (try (char '~' >> commandParser) <|> commentParser <|> Term <$> termParser)

commandParser :: Parser Input
commandParser =  Let                          <$> (insensitive "let"        >> space1 >> ident) <*> (space1 >> string ":=" >> termParser)
             <|> Reds                         <$> (insensitive "reductions" >> space1 >> termParser)
             <|> Eval                         <$> (insensitive "eval"       >> space1 >> stratParser)
             <|> const PPrint                 <$>  insensitive "pprint"
             <|> Script                       <$> (insensitive "script"     >> space1 >> some anyChar)
             <|> const (Script "lib/Prelude") <$>  insensitive "prelude"
             <|> const Help                   <$>  insensitive "help"
             <|> const Exit                   <$>  insensitive "exit"

ident :: Parser String
ident = do (x:xs) <- some (satisfy isValid)
           guard (not (isLower x || isDigit x))
           pure (x:xs)

commentParser :: Parser Input
commentParser =  const Comment <$> (string "~~" >> many anyChar)
             <|> const Comment <$> eof

stratParser :: Parser EvalStrat
stratParser =  const Norm <$> insensitive "norm"
           <|> const Appl <$> insensitive "appl"
           <|> const Off  <$> insensitive "off"

{-
    The λ-term parser initially parses NATs and IDENTs as variables, so we must deference
    them with respect to a interpreter environment to obtain the actual λ-terms they represent.
-}

dereference :: Map String Term -> Term -> Maybe Term
dereference e (Var x) | all isLower x = pure (Var x)
                      | all isDigit x = pure (toNumeral (read x))
                      | otherwise     = M.lookup x e
dereference e t       = descendM (dereference e) t

toNumeral :: Int -> Term
toNumeral n = Abs "f" (Abs "x" (go n))
  where
    go 0 = Var "x"
    go n = App (Var "f") (go (n-1))
