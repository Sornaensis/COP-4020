module LabmdaCalc where 

import Prelude hiding (id)

--- | Function application
--- | In haskell, all function application is whitespace. 
--- | Function application is a core concept and understanding it explicitly makes haskell easy to understand

--- | The identity function defined as a lambda expression. 
--- | I'll use lambdas to demonstrate application for the next few functions
--- | We don't need to worry about types
--- | The identity function in haskell
id = \x -> x
--- | 'B combinator', function composition
b f g x = f . g $ x 
b' = \f g x -> f (g x) --- | Can be re-written a bit more clearly like this
--- | 'K combinator', drop the second term
k x y = x
k' = \x y -> x
--- | Conditional
cond p f g x = if p x then f x else g x
cond' = \p f g x -> if p x then f x else g x
--- | And so on...
--- | Lambda expressions use the concept of reduction
--- | e.g.
--- | (\x -> x) is a function that takes a value x and returns it to us. 
--- | To 'reduce' the expression we first APPLY something to it.
--- | For example if we say
--- | (\x -> x) 10 
--- | We are said to be APPLYING the value 10 to our lambda expression. 
--- | When we reduce an expression we take the LEFT MOST lambda expression,
--- | and the first expression adjacent to it, from left to right. 
--- | In this case our first lambda expression is the parenthesized expression.
--- | So we take the first expression adjacent to it, from left to right, 
--- | and replace the leftmost variable with this expression everywhere it occurs
--- | within the lambda body. You can think of reduction like this:
--- | (\x -> x) 10 becomes
--- | (\10 -> 10)
--- | Which trivially reduces to 
--- | 10
--- | A slightly better example:
--- | (\f x -> f x) is a function that takes two parameters and applies the first to the second
--- | (\f x -> f x) succ 1  using the principle of reduction we get
--- |(\x -> succ x) 1
--- | succ 1
--- | 2
--- | Every expression in haskell works exactly like this. Syntactic sugar aside.
--- | (\f g x -> f (g x))  function composition, also called a combinator. We use this two compose the input of one function to the next
--- | (\f g x -> f (g x)) (\x -> x / 2) (\x -> x + 5) (7)  <-- All outer parenthesized terms are lambda expressions, so we reduce them..
--- | (\g x -> (\x -> x / 2) (g x)) (\x -> x + 5) (7)
--- | (\x -> (\x -> x / 2) ((\x -> x + 5) x)) 7
--- | IMPORTANT STEP:
--- | (\x -> x / 2) ((\x -> x + 5) 7)
--- |!!! Notice that we only replaced the occurences of 'x' within the lambda body it was bound in. We do not replace values !!!
--- |!!! within other lambda expressions because they have not had anything applied to them. !!!
--- | Now we reduce again, putting the entire expression on the right into the one on the left
--- | ((\x -> x + 5) 7) / 2
--- | (7 + 5) / 2
--- | 6.0

--- | On (.) and ($)
--- | It's a good idea to get used to using both. The general pattern is:
mathStuff :: [Double] -> Double
mathStuff xs = sum . map (uncurry (/)) . zip sqs . reverse $ sqs
--- |            ^--------------------------------------^  ^
--- |                  |                                    \
--- |                   `- A 'new' function which             `- Apply our argument, sqs, to this new function
--- |                      is made up of the steps
--- |                      we want to accomplish.
--- | You can see the pattern is to collapse the LHS with (.) and then apply the RHS with ($)
--- | sum . map (uncurry (/)) . zip sqs . reverse $ sqs
--- | Without the ($) is written as
--- | (sum . map (uncurry (/)) . zip sqs . reverse) sqs
--- | Both are equivalent to:
--- | sum (map (uncurry (/)) (zip sqs (reverse sqs)))
             where
             sqs = map (**2) xs

--- | Below is a much more sophisticated example of application using a foldl technique that demonstrates the power of this concept
--- | of function and function application.

--- | Use foldr to implement foldl
--- | This method constructs a function from every element
--- | of the list and passes the initial value s backwards to
--- | turn a right-associative expression into a left-associative expression
leftFold :: (a -> b -> a) -> a -> [b] -> a
leftFold f s xs = foldr (\v k z -> k $ f z v) id xs s

--- | e.g. let s = 5; f = (-) in foldr (\v k z -> k $ f z v) id [1,2,3] s
--- | I wasn't sure how clear this would be so I went ahead and replaced all occurences of f with (-)
--- | (\v k z -> k $ (-) z v) 1 $ foldr (\v k z -> k $ (-) z v) id [2,3]
--- | (\v k z -> k $ (-) z v) 1 $ (\v k z -> k $ (-) z v) 2 $ foldr (\v k z -> k $ (-) z v) id [3]
--- | (\v k z -> k $ (-) z v) 1 $ (\v k z -> k $ (-) z v) 2 $ (\v k z -> k $ (-) z v) 3 $ id
--- | (\v k z -> k $ (-) z v) 1 $ (\v k z -> k $ (-) z v) 2 $ (\v k z -> k $ (-) z v) 3 id
--- | (\v k z -> k $ (-) z v) 1 $ (\v k z -> k $ (-) z v) 2 (\z -> id $ (-) z 3)
--- | (\v k z -> k $ (-) z v) 1 (\z -> (\z -> id $ (-) z 3) $ (-) z 2)
--- | (\z -> (\z -> (\z -> id $ (-) z 3) $ (-) z 2) $ (-) z 1)
--- | Remember that s at the top?
--- | (\z -> (\z -> (\z -> id $ (-) z 3) $ (-) z 2) $ (-) z 1) s
--- | s = 5
--- | (\z -> (\z -> id $ (-) z 3) $ (-) z 2) $ (-) 5 1
--- | (\z -> (\z -> id $ (-) z 3) $ (-) z 2) 4
--- | (\z -> id $ (-) z 3) $ (-) 4 2
--- | (\z -> id $ (-) z 3) 2
--- | id $ (-) 2 3
--- | id $ (-1)
--- | -1
--- | The point is, we used a right fold to make the expression left-associative! :D

-- | Kinda silly but whatever it's cool. If you can understand why this works, the rest of haskell
-- | is basically just the type system. 
-- | K combinator and conditional to implement filterFirst
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []     = []
filterFirst p (x:xs) = let k x y = x in (\p f g x -> if p x then f x else g x) p (flip (:) (filterFirst p xs)) (k xs) x
