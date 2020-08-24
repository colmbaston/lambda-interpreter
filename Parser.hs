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

data Input = Term  Term
           | Let String Term
           | Reds  Term
           | Time  Term
           | Count Term
           | Script FilePath
           | Eval EvalStrat
           | PPrint
           | Help
           | Exit
           | Comment

data EvalStrat = Norm
               | Appl
               | Off

instance Show EvalStrat where
  show Norm = "norm"
  show Appl = "appl"
  show Off  = "off"

-- this keeps chaning across different versions, so I define it myself
nextChar :: Parser Char
nextChar = satisfy (const True)

parseInput :: Map String Term -> String -> Maybe Input
parseInput env str = case parseMaybe inputParser str of
                       Nothing        -> Nothing
                       Just (Term  t) -> Term  <$> dereference env t
                       Just (Reds  t) -> Reds  <$> dereference env t
                       Just (Time  t) -> Time  <$> dereference env t
                       Just (Count t) -> Count <$> dereference env t
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
insensitive = try . mapM (\c -> char (toUpper c) <|> char (toLower c) >> pure c)

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
                           x <- trim (some (satisfy isVarChar))
                           char '.'
                           Abs x <$> termParser

appLevel :: Parser Term
appLevel = chainl1 atomLevel (App <$ space1)

atomLevel :: Parser Term
atomLevel = Var <$> some (satisfy isIdentChar) <|> parens absLevel

isIdentChar :: Char -> Bool
isIdentChar c = isAscii c && isAlphaNum c

isVarChar :: Char -> Bool
isVarChar c = isAscii c && isLower c

{-
    Parser for interpreter inputs. Commands are strings beginning with a single '~', comments
    contain only spaces or begin with two '~', and anything else is a λ-term.
-}

inputParser :: Parser Input
inputParser = trim (try (char '~' >> commandParser) <|> commentParser <|> Term <$> termParser)

commandParser :: Parser Input
commandParser =  Let                    <$> (insensitive "let"        >> space1 >> ident) <*> (space1 >> string ":=" >> termParser)
             <|> Reds                   <$> (insensitive "reductions" >> space1 >> termParser)
             <|> Time                   <$> (insensitive "time"       >> space1 >> termParser)
             <|> Count                  <$> (insensitive "count"      >> space1 >> termParser)
             <|> Script                 <$> (insensitive "script"     >> space1 >> reverse . dropWhile isSpace . reverse <$> some nextChar)
             <|> Script "./lib/prelude" <$   insensitive "prelude"
             <|> Eval                   <$> (insensitive "eval"       >> space1 >> stratParser)
             <|> PPrint                 <$   insensitive "pprint"
             <|> Help                   <$   insensitive "help"
             <|> Exit                   <$   insensitive "exit"

ident :: Parser String
ident = (:) <$> satisfy (\c -> isAscii c && isUpper c) <*> many (satisfy isIdentChar)

commentParser :: Parser Input
commentParser =  Comment <$ (string "~~" >> many nextChar)
             <|> Comment <$  eof

stratParser :: Parser EvalStrat
stratParser =  Norm <$ insensitive "norm"
           <|> Appl <$ insensitive "appl"
           <|> Off  <$ insensitive "off"

{-
    The λ-term parser initially parses NATs and IDENTs as variables, so we must deference
    them with respect to a interpreter environment to obtain the actual λ-terms they represent.
-}

dereference :: Map String Term -> Term -> Maybe Term
dereference env (Var x) | all isLower x = pure (Var x)
                        | all isDigit x = pure (toNumeral (read x))
                        | otherwise     = M.lookup x env
dereference env t                       = descendA (dereference env) t

toNumeral :: Int -> Term
toNumeral n = Abs "f" (Abs "x" (go n))
  where
    go 0 = Var "x"
    go n = App (Var "f") (go (n-1))
