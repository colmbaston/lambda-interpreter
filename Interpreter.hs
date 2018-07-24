{-# LANGUAGE LambdaCase #-}

import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Map (Map)

import System.IO
import System.Directory
import System.Exit

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline
import System.Console.Haskeline.History

import Lambda
import Parser
import Printer

data InterpreterState = I { env :: Map String Term, strat :: EvalStrat, pprint :: Bool }
type Interpreter      = InputT (StateT InterpreterState IO)

settings :: Settings (StateT InterpreterState IO)
settings = Settings (completeWord Nothing " " completions) Nothing False
  where
    completions :: String -> StateT InterpreterState IO [Completion]
    completions xs@('~':_) = pure (makeComps (prefixes xs ["~eval", "~exit", "~help", "~let", "~pprint", "~prelude", "~reductions", "~script"]))
    completions xs         = do ks <- prefixes xs . M.keys . env <$> get
                                let l = length ks
                                if l >= 50
                                  then do liftIO (putStr ("\ESC[s" ++ red ("\npress y to display all " ++ show l ++ " options")))
                                          c <- liftIO getChar
                                          liftIO (putStr "\ESC[1K\ESC[1T\ESC[u")
                                          pure (if c == 'y' || c == 'Y' then makeComps ks else [])
                                  else pure (makeComps ks)

    prefixes :: String -> [String] -> [String]
    prefixes xs = filter ((map toLower xs `isPrefixOf`) . map toLower)

    makeComps :: [String] -> [Completion]
    makeComps = map (\x -> Completion x (green x) True)

getInput :: MonadException m => InputT m (Maybe String)
getInput = getInputLine (cyan "~> ") >>= \case
                                           Nothing -> pure Nothing
                                           Just  x -> do unless (all isSpace x)
                                                           (modifyHistory (addHistoryUnlessConsecutiveDupe x))
                                                         pure (Just x)

interruptible :: MonadException m => InputT m a -> InputT m ()
interruptible x = handle (\Interrupt -> outputStrLn (red "interrupted")) (withInterrupt (void x))

red :: String -> String
red x = "\ESC[1;31m" ++ x ++ "\ESC[m"

green :: String -> String
green x = "\ESC[1;32m" ++ x ++ "\ESC[m"

cyan :: String -> String
cyan x = "\ESC[1;36m" ++ x ++ "\ESC[m"

getPrint :: Bool -> (Term -> String)
getPrint True  = pShow
getPrint False = show

getEval :: Bool -> EvalStrat -> (Term -> Term)
getEval _     Off  = id
getEval True  Norm = normEval
getEval False Norm = normOrder
getEval True  Appl = applEval
getEval False Appl = applOrder

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          intro
          evalStateT (runInputT settings (forever interpret)) (I M.empty Norm True)

intro :: IO ()
intro = do putStrLn ('\n' : cyan ascii ++ "\n")
           putStrLn (green "                   An Interpreter for the Untyped λ-Calculus")
           putStrLn (green "                     Enter \"~help\" for more information...")
           putStrLn ""

ascii :: String
ascii = "     ______                __                           __        ___      __  \n\
        \    / ____/_  ______  ____/ /___ _____ ___  ___  ____  / /_____ _/ (_)____/ /_ \n\
        \   / /_  / / / / __ \\/ __  / __ `/ __ `__ \\/ _ \\/ __ \\/ __/ __ `/ / / ___/ __/ \n\
        \  / __/ / /_/ / / / / /_/ / /_/ / / / / / /  __/ / / / /_/ /_/ / / (__  ) /_   \n\
        \ /_/    \\__,_/_/ /_/\\__,_/\\__,_/_/ /_/ /_/\\___/_/ /_/\\__/\\__,_/_/_/____/\\__/   \n\
        \                 ______                 __  _                   __             \n\
        \                / ____/_  ______  _____/ /_(_)___  ____  ____ _/ /             \n\
        \               / /_  / / / / __ \\/ ___/ __/ / __ \\/ __ \\/ __ `/ /              \n\
        \              / __/ / /_/ / / / / /__/ /_/ / /_/ / / / / /_/ / /               \n\
        \             /_/    \\__,_/_/ /_/\\___/\\__/_/\\____/_/ /_/\\__,_/_/                \n\
        \        ____                                                  _                \n\
        \       / __ \\_________  ____ __________ _____ ___  ____ ___  (_)___  ____ _    \n\
        \      / /_/ / ___/ __ \\/ __ `/ ___/ __ `/ __ `__ \\/ __ `__ \\/ / __ \\/ __ `/    \n\
        \     / ____/ /  / /_/ / /_/ / /  / /_/ / / / / / / / / / / / / / / / /_/ /     \n\
        \    /_/   /_/   \\____/\\__, /_/   \\__,_/_/ /_/ /_/_/ /_/ /_/_/_/ /_/\\__, /      \n\
        \                     /____/                                       /____/       \n"

interpret :: Interpreter ()
interpret = do e <- env <$> lift get
               (parseInput e =<<) <$> getInput >>= maybe parseError runInput

parseError :: Interpreter ()
parseError = outputStrLn (red "parse error")

runInput :: Input -> Interpreter ()
runInput i = do s <- lift get
                case i of
                  Term t   -> interruptible (runTerm t)
                  Reds t   -> interruptible (runReds t)
                  Let n t  -> lift (put s{ env = M.insert n t (env s) })
                  Script f -> runScript f
                  Eval e   -> lift (put s{ strat = e })
                  PPrint   -> runPPrint
                  Help     -> runHelp
                  Exit     -> liftIO exitSuccess
                  Comment  -> pure ()

runTerm :: Term -> Interpreter ()
runTerm t = do (e,p) <- (getEval True . strat &&& (getPrint . pprint)) <$> lift get
               outputStrLn (p (e t))

runReds :: Term -> Interpreter ()
runReds t = do (e,p) <- (getEval False . strat &&& getPrint . pprint) <$> lift get
               zipWithM_ (\n -> outputStrLn . (green (show n ++ ": ") ++) . p) [0..] (reductions e t)

runScript :: FilePath -> Interpreter ()
runScript f = do outputStrLn (green "attempting to run script " ++ cyan f)
                 ex <- liftIO (doesFileExist f)
                 if ex
                    then lines <$> liftIO (readFile f) >>= foldr
                                                             (\l x -> uncurry runLine l >>= flip when x)
                                                             (outputStrLn (green "script exited successfully")) . zip [1..]
                    else outputStrLn (red "script " ++ cyan f ++ red " does not exist")

runPPrint :: Interpreter ()
runPPrint = do s <- lift get
               lift (put (s { pprint = not (pprint s) }))
               outputStrLn (green ("pretty printing " ++ if pprint s then "off" else "on"))

runLine :: Int -> String -> Interpreter Bool
runLine n l = do e <- env <$> lift get
                 case parseInput e l of
                   Nothing -> outputStrLn (red ("parse error on line " ++ show n ++ " - stopping")) >> pure False
                   Just  i -> runInput i                                                            >> pure True

runHelp :: Interpreter ()
runHelp = mapM_ (outputStrLn . green) [""
                                      ,"  Available commands:"
                                      ,"    t             -> evaluate λ-term t, outputting the final result (if any)"
                                      ,"    ~reductions t -> evaluate λ-term t, outputting intermediate reductions"
                                      ,"    ~let n := t   -> place λ-term t into the environment, bound to name n"
                                      ,"    ~script f     -> run a script of interpreter commands"
                                      ,"    ~prelude      -> shorthand for \"~script lib/Prelude\""
                                      ,"    ~eval e       -> switch to evaluation strategy e"
                                      ,"    ~pprint       -> toggle pretty-printing between on (the default) and off"
                                      ,"    ~help         -> output this message"
                                      ,"    ~~            -> treat this line as a comment, ignoring it"
                                      ,"    ~exit         -> exit the interpreter"
                                      ,""
                                      ,"  Syntax:"
                                      ,"    A λ-term t may be -> a λ-abstraction \"λv.t\" or \"\\v.t\""
                                      ,"                      -> an application \"t t\""
                                      ,"                      -> a variable \"v\""
                                      ,"                      -> enclosed in parentheses \"(t)\""
                                      ,""
                                      ,"    A variable v may be any non-empty string of lower-case letters. λ-terms"
                                      ,"    placed into the environment by ~let may be referenced by name, and a name n"
                                      ,"    may be any non-empty string of characters from ASCII range 33-125 (except "
                                      ,"    '(', ')', '\\', and '.') which does not begin with a lower-case letter or a"
                                      ,"    numeric character. Identifiers containing only numeric characters are"
                                      ,"    treated as natural numbers and desugared into the corresponding Church"
                                      ,"    numeral representation."
                                      ,""
                                      ,"  Evaluation strategies:"
                                      ,"    norm -> normal order evaluation (the default)"
                                      ,"    appl -> applicative order evaluation"
                                      ,"    off  -> no evaluation"
                                      ,""]
