{-# LANGUAGE ExistentialQuantification #-}
module Shape where

totalArea :: (Floating a) => [SB a] -> a
totalArea = foldr (\(SB x) y -> (+) y . area $ x) 0  

data SB a = forall s. (Floating a, Show (s a), Shape s) => SB (s a) 

instance (Show a) => Show (SB a) where
    show (SB s) = show s

class Shape s where
    area :: (Floating a) => s a -> a

data Rectangle a = Rectangle { height :: a, width :: a } deriving Show
data Circle a    = Circle { radius :: a } deriving Show

instance Shape Rectangle  where
    area (Rectangle h w) = h*w

instance Shape Circle where
    area (Circle r) = pi*r**2
