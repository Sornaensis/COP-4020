{-# LANGUAGE FlexibleContexts #-}
module Lamb.Repl where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char (isSpace,toLower)
import Lamb.Parser
import Lamb.Language
import System.Console.Haskeline 

type ReplState = StateT ReplEnv (InputT IO) ()
type ReplEnv   = (Bool, Env)
type ReplFunc  = (String -> ReplState)
type ReplKey   = [String]
type ReplCmd   = (ReplKey, ReplFunc)

initEnv :: ReplEnv
initEnv = (True, [])

--- | Allow us to pass-off the handling of input strings AND
--- | the state of the REPL by packing everything neatly into
--- | a StateT monad
processCmd :: String -> ReplState
processCmd input = let cmd  = map toLower . takeWhile (/= ' ') $ input
                       args = drop (length cmd + 1) input
                   in case cmdLookup cmd replCmds of
                         Nothing -> return ()
                         Just f  -> f args

--- | Look for a command in an associative array of lists of strings
--- | that map to repl command functions
cmdLookup :: String -> [ReplCmd] -> Maybe ReplFunc
cmdLookup key [] = Nothing
cmdLookup key ((keys,f):fs) = if key `elem` keys then Just f else cmdLookup key fs

replCmds :: [ReplCmd]
replCmds = [ ([":load",":l"], 
                replLoadFile
             )
           , ([":s",":show"],
                (\s -> case runLambParser (trim s) of
                        Left err -> lift $ outputStrLn err
                        Right ret -> lift $ outputStrLn (show ret)
                )
             )
           ] 

--- | Load file into our interpreter environment
replLoadFile :: String -> ReplState
replLoadFile fn = do (_,env) <- get
                     env' <- liftIO (readFile (trim fn)) >>= \c -> return $ (procEnv env . lines) c 
                     put (True, env')
                where 
                procEnv env []     = env
                procEnv env (x:xs) = case (runLambParser . trim) x of
                                        Right t@(Rec n f) -> case eval env t of
                                                                Right v -> procEnv (define n v env) xs
                                                                _       -> procEnv env xs
                                        _               -> procEnv env xs

--- | Function that actually evaluates the input for the repl
evalInput :: String -> ReplState
evalInput input = do (_,env) <- get
                     case runLambParser (trim input) of
                      Left err  -> lift $ outputStrLn err
                      Right ret -> case ret of
                                    (Rec n f) -> case eval env ret of
                                                   Right v -> put (True, define n v env)
                                                   Left e  -> lift $ outputStrLn e 
                                    _         -> case eval env ret of
                                                   Right v -> lift $ (outputStrLn . show) v 
                                                   Left e  -> lift $ outputStrLn e

--- | The Read-Eval-Print-Loop
--- | Use state transformer to parameterize the InputT environment to allow us to
--- | easily pass our own state around to various helper functions
lambRepl :: ReplState
lambRepl = do 
            (go, env) <- get
            ln <- lift $ getInputLine "λ> " 
            (case ln of
             Nothing           -> put (False, env)
             Just "quit"       -> put (False, env)
             Just []           -> lambRepl
             Just cmd@(':':_)  -> processCmd cmd
             Just str          -> evalInput str)
        >> get >>= \(run,_) -> when run lambRepl 

-- | Util

dropFromTailWhile _ [] = []
dropFromTailWhile p items
    | p (last items) = dropFromTailWhile p $ init items
    | otherwise      = items
    
trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace                                

