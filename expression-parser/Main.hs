module Main (main) where

import Data.Char        (isSpace)
import Expr.Expression  (ENum, eval)
import Expr.Parser

--- | Use (portable) haskeline for input. 
--- | Provides history that can be scrolled with up/down arrows
--- | and using left/right arrow keys while editing the line
import System.Console.Haskeline

main :: IO ()
main = 
 putStrLn ":: C-style expression parser/evaluator\n:: Type 'quit' to Quit" >> 
 runInputT defaultSettings newMain
 where
 newMain :: InputT IO ()
 newMain =
  getInputLine "expr> "
  >>= \ln -> 
     case ln of
      Nothing     -> newMain
      Just "quit" -> return ()
      Just str    -> 
         (case parseExpr str of
           Left err  -> outputStrLn $ "ERROR: " ++ err
           Right val -> (outputStrLn . show) val >> 
                        case eval val of
                           Left i  -> outputStrLn . show $ i
                           Right b -> outputStrLn . show $ b) >> newMain
