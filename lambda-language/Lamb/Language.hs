module Lamb.Language ( Env
                     , define
                     , TNum(..)
                     , Term(..)
                     , Value(..)
                     , eval) where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

import Data.Maybe

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
prjFun  _      = \_ -> throwError "bad function value"

prjReal :: Value -> Double
prjReal (Num n) = tnum2double n
prjReal (Bool b)  = if b then 1 else 0
prjReal _         = error "Invalid floating point value"

prjInt :: Value -> Integer
prjInt (Num n)  = tnum2int n
prjInt (Bool b) = if b then 1 else 0
prjInt  _       = error "bad integer value"

prjBool :: Value -> Bool
prjBool (Bool b) = b
prjBool (Num n)  = case n of 
                    Int n  -> n /= 0
                    Real d -> abs d > 1e-12
prjBool  _       = error "bad boolean value"

-- environments mapping variables to values

type TError = Either String

throwError :: String -> TError a
throwError = Left

type Env = [(Var, Value)]

getval :: Var -> Env -> TError Value
getval x env = case lookup x env of
                Just val  -> return val
                Nothing   -> throwError $ "No value for " ++ show x

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
eval env (Rec x m) = f 
                    where 
                    f = case f' of 
                         l@(Left _)  -> l
                         (Right f'')  -> eval ((x,f'') : env) m
                    f' = eval env m
                                       

-- a (fixed) "environment" of language primitives

binOp inj f inj' f' = 
    Fun (\i -> return $ 
      Fun (\j -> return $ case i of
           (Num (Real _)) -> inj' $ f' (prjReal i) (prjReal j)
           _              -> case j of
                              (Num (Real _)) -> inj' $ f' (prjReal i) . prjReal $ j
                              _              -> inj  $ f (prjInt i) . prjInt $ j))

toReal = Num . Real
toInt  = Num . Int

numOp f = binOp toInt f toReal

times    = numOp (*) (*)
divide   = numOp div (/)
minus    = numOp (-) (-)
plus     = numOp (+) (+)
equal    = binOp Bool (==) Bool (==)
cond     = Fun (\b -> return $ Fun (\x -> return $ Fun (\y -> return $ if prjBool b then x else y)))

prims :: Env
prims = [ ("+",plus),
          ("*", times), 
          ("/", divide),
          ("-", minus), 
          ("==", equal), 
          ("if", cond), 
          ("True", Bool True),
          ("False", Bool False)]
