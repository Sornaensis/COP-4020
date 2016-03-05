# COP 4020 | Programming Languages

### Contents
+ [What is this?](#what-is-this)
+ [Packages](#packages)

### What is this?

This repo is a collection haskell programs and snippets I have written for/during Spring 2016 COP4020, including homework, lecture notes, and random stuff related to the class. Some prolog may show up here and there as well.

I have some haskell and lambda calculus tutorials written up in the [lambda-calc](#./labmda-calc/) subdirectory. The [Lambda Calculus](#lambda-calculus) is important to understanding functional programming in general, but it is especially relevant to haskell, because haskell is essentially a typed version of lambda calculus.

### Packages
Some of the programs I have written use third-party haskell libraries to accomplish certain things that I would otherwise have to implement all over again. 

Currently that library is just

- [Haskeline](https://hackage.haskell.org/package/haskeline)

It can be installed by following the short guide below:

#### Installing Packages
###### Linux/OSX/BSD Users
In your favorite $shell
```sh
$ cabal update
$ cabal install <package name>
```
###### Windows Users
Press `Windows+R` and type `cmd.exe` and then 
```sh
C:\> cabal update
C:\> cabal install <package name>
```

