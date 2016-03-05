module RegularExpressions where

import Prelude hiding ((<*>))
import Data.Char

type RegExp = String -> Bool

char :: Char -> RegExp
char = (==) . return  --- | Monadic injection is more idiomatic and all that jazz

--- | Set fixity levels to avoid lots of redundant brackets/confusing regexes
--- | Basically    e1 <+> e2 <+> e3 <+> (e4 <||> e5 <+> e6) <||> e7 <+> e8    <--- With fixity
--- |           == (e1 <+> e2 <+> e3 <+> (e4 <||> (e5 <+> e6))) <||> (e7 <+> e8)  <--- Without fixity
infixl 0 <||>, |||
infixl 9 <+>, <*>

--- | When you have infix functions you can use infix pattern matching to make things more clear
(<||>) :: RegExp -> RegExp -> RegExp
(e1 <||> e2) s = e1 s || e2 s

(<+>) :: RegExp -> RegExp -> RegExp
(e1 <+> e2) s = or (do (p,s) <- splits s
                       return (e1 p && e2 s))

--- | I do not like these names, not one bit !!
(|||) = (<||>)
(<*>) = (<+>)

-- | Ain't usin no «splitAt» nonsense!
splits :: String -> [(String, String)]
splits [] = []
splits xs = zip (reverse . map reverse . consum . reverse $ xs) (consum xs) 
            where
            consum []          = [""]
            consum xt@(_:xs)   = xt : consum xs

--- | Match a given string literal idiomatically within a Regex 
literal :: String -> RegExp
literal = (==)

--- | Epsilon / e / ""
nothing :: RegExp
nothing = (==) ""

--- | ? quantifier
option :: RegExp -> RegExp
option e = nothing <||> e 

--- | * quantifier
many :: RegExp -> RegExp
many e = nothing <||> (e <+> many e)
       where
       --- | Fix for infinite loop of recursive many calls
       (<+>) :: RegExp -> RegExp -> RegExp
       (e1 <+> e2) s = or (do (p,s) <- (tail . splits) s
                              return (e1 p && e2 s))
star = many

--- | + quantifier
plus :: RegExp -> RegExp
plus e = e <+> many e
many1 :: RegExp -> RegExp
many1 = plus

--- | Using the primitives above you can represent regexes fairly clearly in haskell

--- | Inverse character class
noneOf :: String -> RegExp
noneOf s = not . oneOf s

--- | Character class
oneOf :: String -> RegExp
oneOf []     = nothing
oneOf (x:xs) = foldr ((<||>) . char) (char x) xs

--- | Latin alphabet
alpha :: RegExp
alpha = oneOf $ ['a'..'z'] ++ ['A'..'Z']

--- | Decimal Digits
digits :: RegExp
digits = oneOf ['0'..'9']

nonZero :: RegExp
nonZero = oneOf ['1'..'9']

--- | Either
alphanum :: RegExp
alphanum = alpha <||> digits

--- | Match natural numbers
number :: RegExp
number = nonZero <+> many digits <||> char '0' --- | ([1-9][0-9]*|0)

--- | Match floating point numbers
fractional :: RegExp
fractional = (char '0' <||> nonZero <+> many digits) <+> char '.' <+> (digits <||> many digits <+> nonZero)
             --- | (0|[1-9][0-9]*)\.([0-9]|[0-9]*[1-9])

--- | Match either floating point numbers or natural numbers
--- | Left non-simplified to demonstrate operator precedence
fractOrNum :: RegExp
fractOrNum = --- | Match a fractional number
             (char '0' <||> nonZero <+> many digits) <+> char '.' <+> (digits <||> many digits <+> nonZero)
             --- | OR
             <||>  
             --- | Match a natural number
             nonZero <+> many digits <||> char '0'

--- | This matches a floating point number which can be read by haskell's «read» function as a Double
--- | The Regex => -?[1-9]\d*(\.(0|[1-9]\d*))?(e[+-]?[1-9]\d*)? will match a string corresponding to
--- | a valid Double precision floating point number (taking into account the redundant zeros restriction)
--- | Example:
--- | ghci> read "1.3e-4" :: Double
--- | 1.3e-4
floating :: RegExp --- | Match any valid floating point number
floating = option (oneOf "-") <+> 
           (char '0' <||> nonZero <+> many digits)
           <+> option (char '.' <+> (char '0' <||> many digits <+> nonZero))
           <+> option (char 'e' <+> option (oneOf "+-") <+> nonZero <+> many digits)

--- | Example main function to demonstrate regex input validation:
main :: IO () 
main = main' []
      where
      main' :: [Double] -> IO ()
      main' xs = getLine >>= \x -> 
                   if floating x                          -- Recursively call main'
                     then let n = (read x :: Double) in print (n:xs)  >> main' (n:xs)
                     else case map toLower x of
                            "quit" -> return ()
                            "q"    -> return ()
                            _      -> putStrLn "Invalid Float Input"   >> main' xs 
 
 
