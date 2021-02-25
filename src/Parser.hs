module Parser where

import           Data.Void
import           Data.Char
import qualified Data.Set as S
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

data Input t = Term       t
             | Let String t
             | Reds       t
             | Script FilePath
             | Eval EvalStrat
             | PPrint
             | Help
             | Exit
             | Comment

data PTerm = PVar String
           | PAbs String PTerm
           | PApp PTerm PTerm
           | PNumeral Integer
           | PPair PTerm PTerm
           | PList [PTerm]

data EvalStrat = Norm
               | Appl
               | Off

instance Show EvalStrat where
  show Norm = "norm"
  show Appl = "appl"
  show Off  = "off"

{-
    λ-Terms are first parsed as PTerms, representing the higher-level concepts of
    numerals, pairs, and lists, and are then desugared to Terms represented using
    nothing more than variables, λ-abstractions, and applications.
-}

parseInput :: Map String Term -> String -> Maybe (Input Term)
parseInput env str = case parseMaybe inputParser str of
                       Nothing          -> Nothing
                       Just (Term  t)   -> Term  <$> desugar env t
                       Just (Reds  t)   -> Reds  <$> desugar env t
                       Just (Let n t)   -> Let n <$> mfilter (null . freeVars) (desugar env t)
                       Just (Script fp) -> Just (Script fp)
                       Just (Eval e)    -> Just (Eval e)
                       Just  PPrint     -> Just  PPrint
                       Just  Help       -> Just  Help
                       Just  Exit       -> Just  Exit
                       Just  Comment    -> Just  Comment

desugar :: Map String Term -> PTerm -> Maybe Term
desugar env (PVar x) | all isLower x = pure (Var x)
                     | otherwise     = M.lookup x env
desugar env (PAbs x y)               = Abs x  <$> desugar env y
desugar env (PApp x y)               = App    <$> desugar env x <*> desugar env y
desugar _   (PNumeral x)             = pure (toNumeral x)
desugar env (PPair x y)              = toPair <$> desugar env x <*> desugar env y
desugar env (PList xs)               = toList <$> mapM (desugar env) xs

toNumeral :: Integer -> Term
toNumeral = Abs ff . Abs fx . go
  where
    go :: Integer -> Term
    go 0 = Var fx
    go n = App (Var ff) (go (n-1))

    ff :: String
    ff = freshName S.empty

    fx :: String
    fx = nextName ff

toPair :: Term -> Term -> Term
toPair x y = Abs fp (App (App (Var fp) x) y)
  where
    fp = freshName (freeVars x `S.union` freeVars y)

toList :: [Term] -> Term
toList xs = Abs ff (Abs fx (go xs))
  where
    go :: [Term] -> Term
    go []     = Var fx
    go (y:ys) = App (App (Var ff) y) (go ys)

    ff :: String
    ff = freshName (foldl (\s x -> freeVars x `S.union` s) S.empty xs)

    fx :: String
    fx = nextName ff

{-
    Some useful parser combinators to augment the set of Megaparsec combinators.
-}

nextChar :: Parser Char
nextChar = satisfy (const True)

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

insensitive :: String -> Parser String
insensitive = try . mapM (\c -> char (toUpper c) <|> char (toLower c) >> pure c)

{-
    The parser for λ-terms, loosely based on the following context-free grammar:

     -> ABS     ::= λ VAR . ABS
                 |  APP

        APP     ::= APP [space] ATOM
                 |  ATOM

        ATOM    ::= VAR
                 |  IDENT
                 |  NUMERAL
                 |  PAIR
                 |  LIST
                 |  ( ABS )

        VAR     ::= [non-empty string of lower-case letters]
        IDENT   ::= [non-empty string of letters, beginning with an upper-case letter]
        NUMERAL ::= [non-empty string of decimal digits]
        PAIR    ::= < ABS , ABS >
        LIST    ::= [ ABS , ... ]
-}

termParser :: Parser PTerm
termParser = trim absLevel

absLevel :: Parser PTerm
absLevel = appLevel <|> do void (char '\\' <|> char 'λ')
                           x <- trim (some (satisfy isVarChar))
                           void (char '.')
                           PAbs x <$> termParser

appLevel :: Parser PTerm
appLevel = chainl1 atomLevel (PApp <$ space1)

atomLevel :: Parser PTerm
atomLevel =  PVar <$> some (satisfy isIdentChar)
         <|> PNumeral . read <$> some (satisfy isDigit)
         <|> do void (char '<')
                x <- termParser
                void (char ',')
                y <- termParser
                void (char '>')
                pure (PPair x y)
         <|> do void (char '[')
                xs <- termParser `sepBy` char ','
                void (char ']')
                pure (PList xs)
         <|> do void (char '(')
                x <- termParser
                void (char ')')
                pure x

isVarChar :: Char -> Bool
isVarChar c = isAscii c && isLower c

{-
    Parser for interpreter inputs. Commands are strings beginning with a single '~', comments
    contain only spaces or begin with two '~', and anything else is a λ-term.
-}

inputParser :: Parser (Input PTerm)
inputParser = trim (try (char '~' >> commandParser) <|> commentParser <|> Term <$> termParser)

commandParser :: Parser (Input PTerm)
commandParser =  Let                    <$> (insensitive "let"        >> space1 >> ident) <*> (space1 >> string ":=" >> termParser)
             <|> Reds                   <$> (insensitive "reductions" >> space1 >> termParser)
             <|> Script                 <$> (insensitive "script"     >> space1 >> reverse . dropWhile isSpace . reverse <$> some nextChar)
             <|> Script "./lib/prelude" <$   insensitive "prelude"
             <|> Eval                   <$> (insensitive "eval"       >> space1 >> stratParser)
             <|> PPrint                 <$   insensitive "pprint"
             <|> Help                   <$   insensitive "help"
             <|> Exit                   <$   insensitive "exit"

ident :: Parser String
ident = (:) <$> satisfy (\c -> isAscii c && isUpper c) <*> many (satisfy isIdentChar)

isIdentChar :: Char -> Bool
isIdentChar c = isAscii c && isAlpha c

commentParser :: Parser (Input PTerm)
commentParser =  Comment <$ (string "~~" >> many nextChar)
             <|> Comment <$  eof

stratParser :: Parser EvalStrat
stratParser =  Norm <$ insensitive "norm"
           <|> Appl <$ insensitive "appl"
           <|> Off  <$ insensitive "off"
