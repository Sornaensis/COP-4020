module Main (main) where

import Control.Monad.State      (evalStateT)
import Lamb.Repl                (lambRepl, initEnv)
import System.Console.Haskeline (runInputT, defaultSettings)

--- | REPL
--- | Example:
--- | λ> let f = \n -> if (== 0 n) 1 (* n (f (- x 1)))
--- | λ> f 4
--- | 24

main :: IO ()
main = do putStrLn "λ :: REPL\nType 'quit' to exit."
          runInputT defaultSettings $ evalStateT lambRepl initEnv

