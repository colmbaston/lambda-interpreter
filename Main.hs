{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Numeric
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Time.Clock

import System.IO
import System.IO.Echo
import System.Exit
import System.Directory
import System.Console.Haskeline
import System.Console.Haskeline.History

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Catch

import Lambda
import Parser
import Printer

{-
    The main module for the program, dealing with all of the IO that needs to be done. Uses the
    Haskeline library's 'InputT' monad transformer to take input from stdin, while also
    providing input history, tab-completion, and a line-editing interface. The interpreter
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

prompt :: String
prompt = "\ESC[1;36m~>\ESC[m "

{-
    Configuration for the 'InputT' monad transformer which controls the
    behaviour of reading lines from stdin, input history, and tab-completion.
-}

getInput :: (MonadIO m, MonadMask m) => InputT m (Maybe String)
getInput = getInputLine prompt >>= \case
                                      Nothing -> pure Nothing
                                      Just  x -> do unless (all isSpace x)
                                                      (modifyHistory (addHistoryUnlessConsecutiveDupe x))
                                                    pure (Just x)

settings :: Settings (StateT InterpreterState IO)
settings = Settings (completeWord Nothing " ()\\." completions) (Just ".history") False
  where
    completions :: String -> StateT InterpreterState IO [Completion]
    completions xs@('~':_) = pure (makeComp (prefixes xs ["~count",
                                                          "~eval",
                                                          "~exit",
                                                          "~help",
                                                          "~let",
                                                          "~pprint",
                                                          "~prelude",
                                                          "~reductions",
                                                          "~script",
                                                          "~time"]))
    completions xs         = do ks <- prefixes xs . M.keys . env <$> get
                                let l = length ks
                                if l >= 50
                                  then do ansiColour red
                                          liftIO (putStr ("\npress y to display all " ++ show l ++ " options"))
                                          ansiColour reset
                                          c <- liftIO (withoutInputEcho getChar)
                                          if toLower c == 'y'
                                            then pure (makeComp ks)
                                            else liftIO (putStr ('\n' :prompt)) >> pure []
                                  else do pure (makeComp ks)

    prefixes :: String -> [String] -> [String]
    prefixes xs = filter ((map toLower xs `isPrefixOf`) . map toLower)

    makeComp :: [String] -> [Completion]
    makeComp = map (\x -> Completion x ("\ESC[1;32m" ++ x ++ "\ESC[m") True)

interruptible :: (MonadIO m, MonadMask m) => InputT m a -> InputT m ()
interruptible x = handle (\Interrupt -> outputStrLn "\ESC[1;31minterrupted" >> ansiColour reset) (withInterrupt (void x))

{-
    The entry-point to the program. Set IO buffering mode, print a welcome message and run
    the infinite loop 'forever interpret' in the interpreter's monad transformer stack.
-}

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          ansiColour cyan
          putStrLn "Fundamentalist Functional Programming"
          ansiColour green
          putStrLn "An Interpreter for Alonzo Church's Untyped λ-Calculus"
          putStrLn "Enter \"~help\" for more information."
          evalStateT (runInputT settings (forever interpret)) (I M.empty Norm True)

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
               getInput >>= maybe parseError runInput . (parseInput e =<<)

parseError :: Interpreter ()
parseError = do ansiColour red
                outputStrLn "parse error"

runInput :: Input Term -> Interpreter ()
runInput i = do s <- lift get
                case i of
                  Term  t  -> interruptible (runTerm  t)
                  Reds  t  -> interruptible (runReds  t)
                  Time  t  -> interruptible (runTime  t)
                  Count t  -> interruptible (runCount t)
                  Let n t  -> lift (put s{ env = M.insert n t (env s) })
                  Script f -> runScript f
                  Eval e   -> lift (put s{ strat = e })
                  PPrint   -> runPPrint
                  Help     -> runHelp
                  Exit     -> liftIO exitSuccess
                  Comment  -> pure ()

{-
    Procedures to run inputs containing λ-terms. If a λ-term is entered, it is fully-evaluated
    and printed according to the current evaluation and printing strategies. If the ~reductions
    command is entered, intermediate reductions are also printed, along with the number of
    reductions taken to reach that point. If the ~time command is entered, the time to
    normalise is displayed.
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
runTerm t = do (e,p) <- (getEval True . strat &&& getPrint . pprint) <$> lift get
               outputStrLn (p (e t))

runReds :: Term -> Interpreter ()
runReds t = do (e,p) <- (getEval False . strat &&& getPrint . pprint) <$> lift get
               zipWithM_ (\n t -> do ansiColour green
                                     outputStr (show n ++ ": ")
                                     ansiColour reset
                                     outputStrLn (p t))
                 ([0..] :: [Int])
                 (reductions e t)

runTime :: Term -> Interpreter ()
runTime t = do (s,p) <- (strat &&& getPrint . pprint) <$> lift get
               start <- liftIO getCurrentTime
               outputStrLn (p (getEval True s t))
               end   <- liftIO getCurrentTime
               ansiColour green
               outputStr "normalised in "
               ansiColour cyan
               outputStr (showFFloat (Just 3) (realToFrac (diffUTCTime end start)) " seconds")
               ansiColour green
               outputStr " with evaluation strategy "
               ansiColour cyan
               outputStrLn (show s)

runCount :: Term -> Interpreter ()
runCount t = do (s,p) <- (strat &&& getPrint . pprint) <$> lift get
                let (le,la) = foldl (\(!le,_) la -> (le+1, la)) (-1, undefined) (reductions (getEval False s) t)
                outputStrLn (p la)
                ansiColour green
                outputStr "normalised in "
                ansiColour cyan
                outputStr (show le)
                outputStr " β-reduction"
                when (le /= 1) (outputStr "s")
                ansiColour green
                outputStr " with evaluation strategy "
                ansiColour cyan
                outputStrLn (show s)

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
                            liftIO (readFile f) >>= foldr
                                                      (\l x -> uncurry runLine l >>= flip when x)
                                                      (do ansiColour green
                                                          outputStrLn "script executed successfully") . zip [1..] . lines
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
                                 outputStr"parse error on "
                                 ansiColour cyan
                                 outputStr ("line " ++ show n)
                                 ansiColour red
                                 outputStrLn " - stopping"
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
               outputStr "pretty-printing "
               ansiColour cyan
               outputStrLn (if pprint s then "off" else "on")

runHelp :: Interpreter ()
runHelp = do ansiColour green
             mapM_ outputStrLn [""
                               ,"  Available commands:"
                               ,"    t             -> evaluate λ-term t, outputting the final result (if any)"
                               ,"    ~reductions t -> evaluate λ-term t, outputting intermediate reductions"
                               ,"    ~let n := t   -> place λ-term t into the environment, bound to name n"
                               ,"    ~script f     -> run a script of interpreter commands"
                               ,"    ~prelude      -> shorthand for \"~script lib/prelude\""
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
                               ,"    may be any non-empty string of letters starting with an upper-case letter."
                               ,"    Identifiers containing only numeric characters are treated as natural numbers"
                               ,"    and desugared into the corresponding Church numeral representation."
                               ,""
                               ,"  Evaluation strategies:"
                               ,"    norm -> normal order evaluation (the default)"
                               ,"    appl -> applicative order evaluation"
                               ,"    off  -> no evaluation"
                               ,""]
