module Eratosthenes where

sieve :: [Integer]
sieve  = sieve' 2 [2..n]
        where
        sieve' n (x:xs) = 
