# COP 4020 | Programming Languages

### Contents
+ [What is this?](#what-is-this)
+ [Packages](#packages)
+ [Lambda Calculus](#lambda-calculus)

## What is this?

This repo is a collection haskell programs and snippets I have written for/during Spring 2016 COP4020, including homework, lecture notes, and random stuff related to the class. Some prolog may show up here and there as well.

I have some haskell and lambda calculus tutorials written up in the [lambda-calc](/labmda-calc/) subdirectory. The [Lambda Calculus](#lambda-calculus) is important to understanding functional programming in general, but it is especially relevant to haskell, because haskell is essentially a typed version of lambda calculus.

## Packages
Some of the programs I have written use third-party haskell libraries to accomplish certain things that I would otherwise have to implement all over again. 

Currently that library is just

- [Haskeline](https://hackage.haskell.org/package/haskeline)

It can be installed by following the short guide below:

### Installing Packages
##### Linux/OSX/BSD Users
In your favorite $shell
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

```haskell
ghci> let compose f g x = f (g x)
ghci> compose (+ 3) (/2) 10
8.0  -- | Haskell has a concept of Fractional vs Integral values, we can ignore that for lambda calc, which is typeless
```

```haskell
compose f g x = f (g x)
compose = \f g x -> f (g x)
compose = (\f -> (\g -> (\x -> f (g x))))
```

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

