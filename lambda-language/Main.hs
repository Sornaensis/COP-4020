module Main (main) where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

import Control.Monad.IO.Class
import Data.Char (isSpace)
import Debug.Trace
import Lamb.Parser
import Lamb.Language
import System.Console.Haskeline

--- | REPL
--- | Example:
--- | λ> let f = \n -> if (== 0 n) 1 (* n (f (- x 1)))
--- | λ> f 4
--- | 24
main :: IO ()
main = do putStrLn "λλλ Lambda Language REPL. λλλ\nType 'quit' to exit."
          runInputT defaultSettings $ runEnv []
     where
     runEnv :: Env -> InputT IO ()
     runEnv env = handle (\Interrupt -> outputStrLn "Exiting..") $ withInterrupt $
                  getInputLine "λ> " >>= \ln -> 
                   case ln of
                    Nothing               -> return ()
                    Just "quit"           -> return ()
                    Just []               -> runEnv env
                    Just (':':'l':' ':fn) -> liftIO (readFile (trim fn)) >>= \c -> (runEnv . procEnv env . lines) c 
                    Just str              -> case doTerm (trim str) of
                                              Left err  -> outputStrLn err >> runEnv env
                                              Right ret -> outputStrLn (show ret) >>
                                                         case ret of
                                                          (Rec n f) -> case eval env ret of
                                                                         Right v -> runEnv $ define n v env
                                                                         Left e  -> outputStrLn e >> runEnv env
                                                          _         -> case eval env ret of
                                                                        Right v -> (outputStrLn . show) v >> runEnv env
                                                                        Left e  -> outputStrLn e >> runEnv env
                where 
                procEnv env []     = env
                procEnv env (x:xs) = case (doTerm . trim) x of
                                        Right t@(Rec n f) -> case eval env t of
                                                                Right v -> procEnv (define n v env) xs
                                                                _       -> procEnv env xs
                                        _               -> procEnv env xs
                                                                  

dropFromTailWhile _ [] = []
dropFromTailWhile p items
    | p (last items) = dropFromTailWhile p $ init items
    | otherwise      = items
    
trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace                                

