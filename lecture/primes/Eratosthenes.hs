module Eratosthenes where

--- | ghci> take 500 sieve
sieve :: [Integer]
sieve = strip [2..]
      where
      strip (x:xs) = x : strip (filter ((/=) 0 . (`mod`x)) xs)
