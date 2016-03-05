module Expr.Expression 
    (Expr(..),
     eval,
     ENum)
    where

data Expr =  Lit Integer 
           | Bool Bool
           | Inc Expr  
           | Dec Expr  
           | Or Expr Expr
           | And Expr Expr
           | Not Expr 
           | Neg Expr
           | Id  Expr
           | Exp Expr Expr
           | Eq  Expr Expr
           | Neq Expr Expr
           | Add Expr Expr 
           | Sub Expr Expr 
           | Mul Expr Expr 
           | Div Expr Expr deriving (Show)

instance Eq Expr where
    e0 == e1  = eval e0 == eval e1

type ENum = Either Integer Bool

eval :: Expr -> ENum
eval (Bool b)    = Right b
eval (Lit n)     = Left  n
eval (Not e)     = Right . not $ evalBool e
eval (Inc e)     = Left  $ evalInt e + 1
eval (Dec e)     = Left  $ evalInt e - 1
eval (Neg e)     = Left  $ (*) (-1) $ evalInt e
eval (Id e)      = Left  $ evalInt e
eval (And e0 e1) = Right $ evalBool e0 && evalBool e1
eval (Or e0 e1)  = Right $ evalBool e0 || evalBool e1
eval (Exp e0 e1) = Left  $ evalInt e0 ^ evalInt e1
eval (Neq e0 e1) = Right $ e0 /= e1 
eval (Eq e0 e1)  = Right $ e0 == e1
eval (Add e0 e1) = Left  $ evalInt e0 + evalInt e1
eval (Sub e0 e1) = Left  $ evalInt e0 - evalInt e1
eval (Mul e0 e1) = Left  $ evalInt e0 * evalInt e1
eval (Div e0 e1) = Left  $ evalInt e0 `div` evalInt e1

evalInt :: Expr -> Integer
evalInt e = case eval e of
            (Left i)      -> i
            (Right True)  -> 1
            (Right False) -> 0

evalBool :: Expr -> Bool
evalBool e = case eval e of
            (Left i)      -> i /= 0
            (Right b)     -> b
