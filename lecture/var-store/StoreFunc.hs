module StoreFunc ( Store,
                   initial,
                   value,
                   update
                 ) where

type Var = String

newtype Store = Store (Var -> Integer)

initial :: Store
initial = Store $ const 0

value :: Store -> Var -> Integer
value (Store sto) = sto 

update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store (\x -> if x == v then n else sto x)
