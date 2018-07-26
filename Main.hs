{-# LANGUAGE LambdaCase #-}

module Main where

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

{-
    The main module for the program, dealing with all of the IO that needs to be done. Uses the
    Haskeline library's 'InputT' monad transformer to take input from stdin, while also
    providing input history, word completion, and a line-editing interface. The interpreter
    also stores its own state: an environment of mappings from identifiers to terms, the
    current evaluation strategy, and the current printing strategy.
-}

data InterpreterState = I { env :: Map String Term, strat :: EvalStrat, pprint :: Bool }
type Interpreter      = InputT (StateT InterpreterState IO)

{-
    ANSI terminal escape sequences to change the foreground
    colour, to erase text, and to change the cursor position.
-}

ansiColour :: MonadIO m => String -> m ()
ansiColour c = liftIO (putStr ("\ESC[" ++ c ++ "m"))

red :: String
red = "1;31"

green :: String
green = "1;32"

cyan :: String
cyan = "1;36"

reset :: String
reset = ""

ansiSave :: MonadIO m => m ()
ansiSave = liftIO (putStr "\ESC[s")

ansiRestore :: MonadIO m => m ()
ansiRestore = liftIO (putStr "\ESC[u")

ansiErase :: MonadIO m => m ()
ansiErase = liftIO (putStr "\ESC[1K")

ansiScroll :: MonadIO m => m ()
ansiScroll = liftIO (putStr "\ESC[1T")

{-
    Configuration for the 'InputT' monad transformer which controls the
    behaviour of reading lines from stdin, input history, and word completion.
-}

getInput :: MonadException m => InputT m (Maybe String)
getInput = getInputLine "\ESC[1;36m~>\ESC[m " >>= \case
                                                     Nothing -> pure Nothing
                                                     Just  x -> do unless (all isSpace x)
                                                                     (modifyHistory (addHistoryUnlessConsecutiveDupe x))
                                                                   pure (Just x)

settings :: Settings (StateT InterpreterState IO)
settings = Settings (completeWord Nothing " " completions) (Just ".history") False
  where
    completions :: String -> StateT InterpreterState IO [Completion]
    completions xs@('~':_) = pure (makeComp (prefixes xs ["~eval",
                                                          "~exit",
                                                          "~help",
                                                          "~let",
                                                          "~pprint",
                                                          "~prelude",
                                                          "~reductions",
                                                          "~script"]))
    completions xs         = do ks <- prefixes xs . M.keys . env <$> get
                                let l = length ks
                                if l >= 50
                                  then do ansiSave
                                          ansiColour red
                                          liftIO (putStr ("\npress y to display all " ++ show l ++ " options"))
                                          c <- liftIO getChar
                                          ansiErase
                                          ansiScroll
                                          ansiRestore
                                          pure (if c == 'y' || c == 'Y' then makeComp ks else [])
                                  else    pure (makeComp ks)

    prefixes :: String -> [String] -> [String]
    prefixes xs = filter ((map toLower xs `isPrefixOf`) . map toLower)

    makeComp :: [String] -> [Completion]
    makeComp = map (\x -> Completion x ("\ESC[1;32m" ++ x ++ "\ESC[m") True)

interruptible :: MonadException m => InputT m a -> InputT m ()
interruptible x = handle (\Interrupt -> do ansiColour red
                                           outputStrLn "interrupted"
                                           ansiColour reset) (withInterrupt (void x))

{-
    The entry-point to the program. Set IO buffering mode, print a welcome message and run
    the infinite loop 'forever interpret' in the interpreter's monad transformer stack.
-}

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          ansiColour cyan
          putStrLn ('\n' : ascii ++ "\n")
          ansiColour green
          putStrLn "                   An Interpreter for the Untyped λ-Calculus"
          putStrLn "                     Enter \"~help\" for more information...\n"
          evalStateT (runInputT settings (forever interpret)) (I M.empty Norm True)

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

{-
   Read a single line from stdin, attempt to parse it in the current
   environment, and dispatch a procedure depending on the input.

   Procedures defined below without any auxiliary definitions are:
     ~let n := t  ->  insert λ-term t into the environment with identifier n
     ~eval e      ->  set the evaluation strategy to e
     ~exit        ->  successfully exit the program
     ~~comment    ->  do nothing
-}

interpret :: Interpreter ()
interpret = do e <- env <$> lift get
               (parseInput e =<<) <$> getInput >>= maybe parseError runInput

parseError :: Interpreter ()
parseError = do ansiColour red
                outputStrLn "parse error"

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

{-
    Procedures to run inputs containing λ-terms. If a λ-term is entered, it is fully-evaluated
    and printed according to the current evaluation and printing strategies. If a λ-term is
    entered with the ~reductions command, intermediate reductions are also printed, along with
    the number of reductions to reach that point.
-}

getEval :: Bool -> EvalStrat -> (Term -> Term)
getEval _     Off  = id
getEval True  Norm = normEval
getEval False Norm = normOrder
getEval True  Appl = applEval
getEval False Appl = applOrder

getPrint :: Bool -> (Term -> String)
getPrint True  = pShow
getPrint False = show

runTerm :: Term -> Interpreter ()
runTerm t = do (e,p) <- (getEval True . strat &&& (getPrint . pprint)) <$> lift get
               outputStrLn (p (e t))

runReds :: Term -> Interpreter ()
runReds t = do (e,p) <- (getEval False . strat &&& getPrint . pprint) <$> lift get
               zipWithM_ (\n t -> do ansiColour green
                                     outputStr (show n ++ ": ")
                                     ansiColour reset
                                     outputStrLn (p t))
                 ([0..] :: [Int])
                 (reductions e t)

{-
    When the ~script command is entered, first establish whether that file exists. If it does,
    right-fold over the lines of the script, parsing each line on the current environment, and
    running the procedure for each line that parses without error. Since later lines of the
    script may depend on ~let bindings introduced earlier in the script, stop execution of the
    script as soon as a single line fails to parse.
-}

runScript :: FilePath -> Interpreter ()
runScript f = do ansiColour green
                 outputStr "attempting to run script "
                 ansiColour cyan
                 outputStrLn f
                 ex <- liftIO (doesFileExist f)
                 if ex
                    then do ansiColour reset
                            lines <$> liftIO (readFile f) >>= foldr
                                                                (\l x -> uncurry runLine l >>= flip when x)
                                                                (do ansiColour green
                                                                    outputStrLn "script exited successfully") . zip [1..]
                    else do ansiColour red
                            outputStr "script "
                            ansiColour cyan
                            outputStr f
                            ansiColour red
                            outputStrLn " does not exist"

runLine :: Int -> String -> Interpreter Bool
runLine n l = do e <- env <$> lift get
                 case parseInput e l of
                   Nothing -> do ansiColour red
                                 outputStrLn ("parse error on line " ++ show n ++ " - stopping")
                                 pure False
                   Just  i -> do runInput i
                                 pure True

{-
    The final two procedures are:
      ~pprint  ->  toggle the pretty-printing state and print whether it's off or on
      ~help    ->  print a help messsage
-}

runPPrint :: Interpreter ()
runPPrint = do s <- lift get
               lift (put (s { pprint = not (pprint s) }))
               ansiColour green
               outputStrLn ("pretty printing " ++ if pprint s then "off" else "on")

runHelp :: Interpreter ()
runHelp = do ansiColour green
             mapM_ outputStrLn [""
                               ,"  Available commands:"
                               ,"    t             -> evaluate λ-term t, outputting the final result (if any)"
                               ,"    ~reductions t -> evaluate λ-term t, outputting intermediate reductions"
                               ,"    ~let n := t   -> place λ-term t into the environment, bound to name n"
                               ,"    ~script f     -> run a script of interpreter commands"
                               ,"    ~prelude      -> shorthand for \"~script lib/Prelude\""
                               ,"    ~eval s       -> switch to evaluation strategy s"
                               ,"    ~pprint       -> toggle pretty-printing between on (the default) and off"
                               ,"    ~help         -> output this message"
                               ,"    ~exit         -> exit the interpreter"
                               ,"    ~~            -> treat this line as a comment, ignoring it"
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
