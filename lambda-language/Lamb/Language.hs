module Lamb.Language ( Env
                     , define
                     , TNum(..)
                     , Term(..)
                     , Value(..)
                     , eval) where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

import Control.Monad
import Data.Maybe
import Data.Either

data Term = Use Var
          | Lit TNum
          | App Term Term
          | Abs Var  Term
          | Rec Var  Term deriving (Show)

data TNum = Int Integer | Real Double deriving (Show)

tnum2int :: TNum -> Integer
tnum2int (Int n)  = n
tnum2int (Real d) = truncate d

tnum2double :: TNum -> Double
tnum2double (Int n)  = fromIntegral n
tnum2double (Real d) = d

type Var  = String

-- a domain of values, including functions

data Value = Num TNum
           | Bool Bool
           | Fun (Value -> TError Value)
           | Let String Value --- | Tacked on for repl

instance Show Value where
  show (Num  n) = case n of 
                    Int n  -> show n
                    Real d -> show d
  show (Bool b) = show b
  show (Let n _) = n ++ " :: Value -> Value"
  show  _  = ""

prjFun :: Value -> TError Value -> TError Value
prjFun (Fun f) = \i -> case i of
                        l@(Left _) -> l
                        (Right v)  -> f v
prjFun  _      = \_ -> throwError "*** Error: Invalid function coercion"

prjReal :: Value -> TError Double
prjReal (Num n) = return $ tnum2double n
prjReal (Bool b)  = return $ if b then 1 else 0
prjReal _         = throwError "*** Error: Invalid floating point value coercion"

prjInt :: Value -> TError Integer
prjInt (Num n)  = return $ tnum2int n
prjInt (Bool b) = return $ if b then 1 else 0
prjInt  _       = throwError "*** Error: Invalid integer value coercion"

prjBool :: Value -> TError Bool
prjBool (Bool b) = return $ b
prjBool (Num n)  = case n of 
                    Int n  -> return $ n /= 0
                    Real d -> return $ abs d > 1e-12
prjBool  _       = throwError "*** Error: Invalid boolean value coercion"

-- environments mapping variables to values

type TError = Either String

throwError :: String -> TError a
throwError = Left

extract :: TError a -> a
extract (Right n) = n

type Env = [(Var, Value)]

getval :: Var -> Env -> TError Value
getval x env = case lookup x env of
                Just val  -> return val
                Nothing   -> throwError $ "*** Error: `" ++ x ++ "` not defined"

hasval :: Var -> Env -> Bool
hasval x env = case lookup x env of
                Just _     -> True
                Nothing    -> False

define :: Var -> Value -> Env -> Env
define x v []            = [(x,v)]
define x v (e@(n,v1):es) = if x == n then (x,v) : es else e : define x v es

-- an environment-based evaluation function

eval :: Env -> Term -> TError Value
eval env (Use c)   = if hasval c env then getval c env else getval c prims
eval env (Lit k)   = return $ Num k
eval env (App m n) = case eval env m of
                        l@(Left _) -> l 
                        (Right i)  -> case eval env n of
                                        l@(Left _)  -> l
                                        j           -> prjFun i j
eval env (Abs x m) = return $ Fun (\v -> eval ((x,v) : env) m)
eval env (Rec x m) = case eval ((x,Fun return):env) m of 
                         l@(Left _)  -> l
                         (Right _)  -> return f
                   where 
                   f = eval' ((x,f) : env) m

--- | Lazy evaluation
eval' :: Env -> Term -> Value
eval' env (Use c)    = if hasval c env then extract $ getval c env else extract $ getval c prims
eval' env (Lit k)    = Num k
eval' env (App m n)  = extract $ prjFun (eval' env m) (Right $ eval' env n)
eval' env (Abs x m)  = Fun (\v -> return $ eval' ((x,v) : env) m)
eval' env (Rec x m)  = f where f = eval' ((x,f) : env) m
                                       

-- a (fixed) "environment" of language primitives

isReal :: Value -> Bool
isReal (Num (Real _)) = True
isReal _              = False

binOp inj f inj' f' = 
    Fun (\i -> return $ 
      Fun (\j -> if isReal i || isReal j
                  then apply prjReal inj' f' i j
                  else apply prjInt  inj  f  i j))
    where
    apply p t f x y = let x' = p x
                          y' = p y
                      in case x' of
                            Left err -> throwError err
                            Right i' -> case y' of
                                         Left err -> throwError err
                                         Right j' -> return . t $ f i' j'

toReal = Num . Real
toInt  = Num . Int

numOp f = binOp toInt f toReal

times    = numOp (*) (*)
divide   = numOp div (/)
minus    = numOp (-) (-)
plus     = numOp (+) (+)
equal    = binOp Bool (==) Bool (==)
cond     = Fun (\b -> return $ Fun (\x -> return $ Fun (\y -> case prjBool b of
                                                               Left err -> throwError err
                                                               Right b' -> return $ if b' then x else y)))

prims :: Env
prims = [ ("+",plus),
          ("*", times), 
          ("/", divide),
          ("-", minus), 
          ("==", equal), 
          ("if", cond), 
          ("True", Bool True),
          ("False", Bool False)]
