module BTree where

import Data.Functor
import Control.Applicative

data Tree a = Nil | Leaf a | Node (Tree a) a (Tree a) --- | Binary Tree, the Leaf constructor just makes typing trees out easier

instance Functor Tree where
    fmap _ Nil              = Nil
    fmap f (Leaf k)         = Leaf $ f k
    fmap f (Node l k r)     = Node (fmap f l) (f k) (fmap f r)

instance Foldable Tree where --- | Foldr takes, for example, a list [1,2,3,4] and a function f and a starting value s, and turns it into this --> 1 `f` (2 `f` (3 `f` (4 `f` s))) | apply the concept to a tree and we get
    foldr f s Nil           = s  --- | Empty tree yields initial value
    foldr f s (Leaf k)      = f k s  --- | Leaf yields f over the Leaf's value and initial value
                            --
                            --- | Slightly more complicated. 
                            --- | Because this is foldr we need an f :: (a -> b -> b) 
                            --- | where a is the type enclosed by our input tree, so
                            --- |  __ Here we recursively call foldr with a new initial value 
                            --- | /   that is the foldr of the GIVEN initial value down the left-hand 
                            --- | V   branch, onto the right hand branch
    foldr f s (Node l k r)  = foldr f (k `f` foldr f s l) r
                            --- |      ^ 
                            --- |      |  here we recursively call foldr down the left branch of the tree, 
                            --- |       ` the resulting value is passed into foldr again as the new initial value 
                            --- |         for the right-hand branch fold
                            
                            --- | Use foldr to implement foldl
    foldl k z0 tree         = foldr (\v f z -> f $ k z v) id tree z0
                            
    length Nil              = 0
    length (Node l _ r)     = 1 + length l + length r

insTree :: Ord a => Tree a -> a -> Tree a
insTree Nil x          = Leaf x
insTree (Leaf x) y     = if y > x then Node Nil x (Leaf y) else Node (Leaf y) x Nil
insTree (Node l x r) y = if y > x then Node l x (insTree r y) else Node (insTree l y) x r

instance Show a => Show (Tree a) where
    show = showtree

showtree :: Show a => Tree a -> String 
showtree t = init . unlines $ showtree' 0 t
            where
            showtree' d Nil             = []
            showtree' d (Leaf k)        = [replicate d '·' ++ "`─ " ++ show k]
            showtree' d (Node l k r)    = let d2 = d + 1 
                                            in  
                                            (replicate d '·' ++ "`─ " ++ show k) :
                                            (showtree' d2 l ++ showtree' d2 r)
