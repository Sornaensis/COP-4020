# COP 4020 | Programming Languages

### Contents
+ [What is this?](#what-is-this)
+ [Packages](#packages)
+ [Lambda Calculus](#lambda-calculus)
+ [Every Sort](#every-sort)

## What is this?

This repo is a collection haskell programs and snippets I have written for/during Spring 2016 COP4020, including homework, lecture notes, and random stuff related to the class. Some prolog may show up here and there as well.

I have some haskell and lambda calculus tutorials written up in the [lambda-calc](https://github.com/Sornaensis/COP-4020/tree/master/lambda-calc) subdirectory. The [Lambda Calculus](#lambda-calculus) is important to understanding functional programming in general, but it is especially relevant to haskell, because haskell is essentially a typed version of lambda calculus.

## Packages
Some of the programs I have written use third-party haskell libraries to accomplish certain things that I would otherwise have to implement all over again. 

Currently that library is just

- [Haskeline](https://hackage.haskell.org/package/haskeline)

It can be installed by following the short guide below:

### Installing Packages
##### Linux/OSX/BSD Users
In your favorite `$shell`
```sh
$ cabal update
$ cabal install <package name>
```
##### Windows Users
Press `Windows+R` and type `cmd.exe` and then 
```sh
C:\> cabal update
C:\> cabal install <package name>
```

## Lambda Calculus

The basis for the functional programming paradigm and so FP languages (Haskell, Javascript, F#, OcaML, ML, Scala, Clojure, Scheme, Common LISP, (Every other kinda LISP), etc.) is the notion of functions as first-class entities by themselves, in the mathematical sense of the word 'function'. That is, a function gives some output for a given input, but each input can only have a single output. A function cannot return multiple values for a single input. In haskell, which is PURELY functional, functions may not produce any side-effects, or alter the state of the program at any time. In haskell, side effects are put into what is called the `IO Monad`. What that means isn't really important right now. The point is that if you define a funtion in haskell, for instance

```haskell
max a b = if a > b then a else b
```

It must ALWAYS return the same output for a given input. The function will never give you a different output. At the same time any function defined in haskell cannot introduce side-effects. A side-effect for instance would be calling this C-function:

```C
int shiftRight(int* val) {
    if(*val == 0) {
        return 0;
    }
    *val = *val >> 4;
    return 1;
}
```

Notice that this function modified the value stored at the address you pass to it. That's called having a side-effect. In haskell functions only evaluate to a new value. These are two important ideas of the lambda calculus, where functions are applied to arguments and then reduced. Nothing is changed. It is very similar to mathematical notation you may have seen before, e.g. `f(x) = x²`. When you determine the value of `f(3)`, you are binding the name `x` to the value `3` and producing a result. You cannot change the value that you put into the function-- `3` is still `3`, you merely _receive a new value_. Another core concept to functional programming.

#### Lambdas

So, what the heck is a lambda (λ) anyway? Well [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) was invented by a guy named Alonzo Church and it is used as a way of denoting computation mathematically. 

It works like this:

```lambda
λx.x
```

The `λ` indicates the start of a lambda expression. Everything between the `λ` and the `.` are the variables that this particular lambda takes. In this case we only take one, call it x. This term is equivalent to the mathematical expression `f(x)=x`. We put in a value, and get that same value back. It is called the `identity function`.

When we apply a function to an argument we write it like this:

```lambda
(λx.x) 10
```

When we have an expression of this form we can reduce it by binding the variables to the values we are applying the function to, in this case `x` is bound to `10`. We then re-write as

```lambda
(10.10)
```

Whenever we are done binding variables we then drop the `head` of the lambda expression, which is everything we had written between the `λ` and `.`. So we simply end up with

```lambda
10
```

Another example:

```lambda
(λz.(λx.z+x)) 4 5
Apply to first argument, 4
(λx.z+x) 5
Then second
(4+5)
```

[More Examples](/lambda-calc/LambdaCalc.hs)

By replacing the occurrences of `x` in the body with `10`. For the rest of this page I'll write lambdas in haskell syntax:

```haskell
ghci> let f x = x
--- | Equivalent to
ghci> let f = \x -> x
```

In haskell a lambda is just like any function and can take multiple values

```haskell
ghci> let s = \x y -> x --- | Drop second parameter
```

#### Reduction

When working with functions there is a concept of reduction which is a lot like evaluating an expression in mathematics. If you are given the statement `3*4/2+12` you reduce this expression to the _value_ `18`. The same concept applies here.

```haskell
--- | Our function on the left, each argument on the right
ghci> (\a b c d -> a**2 + 2*b - c / d) 10 5 87 4
88.25
```

In haskell we also have the concept of passing functions into other functions

```haskell
--- | Our function on the left, each argument on the right
--- | Now, notice that we are applying a function `b` to the argument `c`
ghci> (\a b c d -> a**2 + b c / d) 10 ((-) 5) 87 4
79.5
```

We can write `((-) 5)` as the lambda `(\x -> 5 - x)`

More passing functions around:

```haskell
ghci> let compose f g x = f (g x)
ghci> compose (+ 3) (/2) 10
8.0  -- | Haskell has a concept of Fractional vs Integral values, we can ignore that for lambda calc, which is typeless
```

In haskell, when we declare a function that takes arguments we are really declaring a series of functions, that each take a single argument. For instance the `compose` function defined above ends up looking like this:

```haskell
compose f g x = f (g x)
compose = \f g x -> f (g x)
compose = (\f -> (\g -> (\x -> f (g x))))
```

So keeping in ming the rules of reduction, we can see why this works with an example:

```haskell
ghci> compose (+ 3) (/ 2) 10
```

```haskell
(\f -> (\g -> (\x -> f (g x)))) (+ 3) (/ 2) 10
```

```haskell
(\f -> (\g -> (\x -> f (g x)))) (+ 3) (/ 2) 10
(\g -> (\x -> (+ 3) (g x))) (/ 2) 10
(\x -> (+ 3) ((/ 2) x)) 10
(+ 3) ((/ 2) 10)
```

Haskell is capable of parsing infix expressions to make typing things out easier than in languages like LISP. This applies to partial-application, so for the functions `(+)` and `(/)` we remember that their defintions look like this:

```haskell
(+)    = \a b -> a  + b
(/)    = \a b -> a  / b
(+ 1)  = \a   -> a  + 1
(1 +)  = \b   -> 1  + b
(/ 2)  = \a   -> a  / 2
(10 /) = \b   -> 10 / b
```

So you can see that when we apply them as infix operations we are 'fixing' one of the variables and leaving the other up to the application.

So if we look at the first expression here we can turn each into a lambda and figure out what's going on:

```haskell
(+ 3) ((/ 2) 10)
(\a -> a + 3) ((\a -> a / 2) 10)
((\a -> a / 2) 10) + 3
(10 / 2) + 3
5 + 3
8
```

So you can see we reduced this expression into an arithmetic expression, which is easy to evaluate of course and get `8`.

## Every Sort

Using typeclasses in haskell we can easily write every single sort, each of which can sort a list of any type of data that `instance`s the typeclass `Ord`

[More info on Ord](https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Ord.html)

#### Bubble Sort
```haskell
bubbleSort :: Ord a => [a] -> [a]
bubbleSort []  = []
bubbleSort xs  = bubbleSort' 0 xs
              where
              bubbleSort' n xs | n < length xs = bubbleSort' (n+1) (bubble' xs)
                               | otherwise     = xs
              bubble' (x0:x1:xs) = if x0 < x1 then x0 : (bubble' (x1:xs)) else x1 : (bubble' (x0:xs))
              bubble' xs         = xs

```
#### Insertion Sort
```haskell
insertSort :: Ord a => [a] -> [a]
insertSort = foldr (\x y -> insert x y)) []
          where
          insert y []        = [y]
          insert y xt@(x:xs) = if y < x 
                                   then y:xt 
                                   else x : insert y xs
```
#### Merge Sort
```haskell
mergeSort :: Ord a => [a] -> [a]
mergeSort xt@(_:_:_) = let l = length xt `div` 2
                           fs = take l xt
                           ts = drop l xt
                       in merge (mergeSort fs) (mergeSort ts)
                     where
                     merge [] []               = []
                     merge xs []               = xs
                     merge [] ys               = ys
                     merge xt@(x:xs) yt@(y:ys) = if x < y then x : merge xs yt else y : merge xt ys
mergeSort x          = x
```
#### Quick Sort
```haskell
quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = let gte = quickSort [z | z <- xs, z >= x]
                       lt = quickSort [z | z <- xs, z < x]
                    in lt ++ [x] ++ gte
```

#### Our own data to Sort
