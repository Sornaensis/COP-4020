module Lamb.Language (Env, define, Term(..), Value(..), eval) where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

-- a dynamically-typed term language

import Data.Maybe

data Term = Use Var
          | Lit Integer
          | App Term Term
          | Abs Var  Term
          | Rec Var  Term

type Var  = String

-- a domain of values, including functions

data Value = Num Integer
           | Bool Bool
           | Fun (Value -> Value)
           | Let String Value --- | Tacked on for repl

instance Show Value where
  show (Num  n) = show n
  show (Bool b) = show b
  show (Let n _) = n ++ " :: Value -> Value"
  show  _  = ""

prjFun :: Value -> Value -> Value
prjFun (Fun f) = f
prjFun  _      = error "bad function value"

prjNum :: Value -> Integer
prjNum (Num n)  = n
prjNum (Bool b) = if b then 1 else 0
prjNum  _       = error "bad integer value"

prjBool :: Value -> Bool
prjBool (Bool b) = b
prjBool (Num n)  = n /= 0
prjBool  _       = error "bad boolean value"

binOp inj f = Fun (\i -> Fun $ inj . f (prjNum i) . prjNum )

-- environments mapping variables to values

type Env = [(Var, Value)]

getval :: Var -> Env -> Value
getval x env = fromMaybe (error ("no value for " ++ x)) (lookup x env) 

hasval :: Var -> Env -> Bool
hasval x env = case lookup x env of
                Just _     -> True
                Nothing    -> False

define :: Var -> Value -> Env -> Env
define x v []            = [(x,v)]
define x v (e@(n,v1):es) = if x == n then (x,v) : es else e : define x v es

-- an environment-based evaluation function

eval :: Env -> Term -> Value
eval env (Use c)   = if hasval c env then getval c env else getval c prims
eval env (Lit k)  = Num k
eval env (App m n) = prjFun (eval env m) (eval env n)
eval env (Abs x m) = Fun  (\v -> eval ((x,v) : env) m)
eval env (Rec x m) = f where f = eval ((x,f) : env) m


-- a (fixed) "environment" of language primitives

times = binOp Num  (*)
minus = binOp Num  (-)
plus  = binOp Num  (+)
equal = binOp Bool (==)
cond  = Fun (\b -> Fun (\x -> Fun (\y -> if prjBool b then x else y)))

prims :: Env
prims = [ ("+",plus),
          ("*", times), 
          ("-", minus), 
          ("==", equal), 
          ("if", cond), 
          ("True", Bool True),
          ("False", Bool False)]
