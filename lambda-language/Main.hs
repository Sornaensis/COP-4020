module Main (main) where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

-- a dynamically-typed term language

import Control.Monad.IO.Class
import Data.Char (isSpace)
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
                    Nothing               -> runEnv env
                    Just "quit"           -> return ()
                    Just (':':'l':' ':fn) -> liftIO (readFile (trim fn)) >>= \c -> (runEnv . procEnv env . lines) c 
                    Just str              -> case doTerm env (trim str) of
                                              Left err  -> outputStrLn err >> runEnv env
                                              Right ret -> case ret of
                                                          (Let n f) -> runEnv $ define n f env
                                                          _         -> (outputStrLn . show) ret >> runEnv env
                where 
                procEnv env []     = env
                procEnv env (x:xs) = case (doTerm env . trim) x of
                                        Right (Let n f) -> procEnv (define n f env) xs
                                        Right _         -> procEnv env xs
                                                                  

dropFromTailWhile _ [] = []
dropFromTailWhile p items
    | p (last items) = dropFromTailWhile p $ init items
    | otherwise      = items
    
trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace                                

