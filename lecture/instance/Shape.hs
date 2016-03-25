{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
module Shape where

--- | Class of (s :: * -> *) over the class Num
--- | Define methods for shapes such as their area, etc
class Shape s where
    -- s is parameterized over some Num a
    area :: (Num a) => s a -> a

--- | Rectangle 
data Rectangle a = Rectangle { height :: a, width :: a } deriving Show

instance Shape Rectangle  where
    area (Rectangle h w) = h*w

--- | FUNKY MAGIC
--- | Because the area of a circle is related to «pi» and «pi» is of type Floating a => a
--- | We can only talk about circles in the context of floating point numbers
--- | So we add a constraint to the constructor, and a stand alone deriving instance
--- | that tells the compiler how to generate Show instances for the datatype
data Circle a    = (Floating a) => Circle { radius :: a }
deriving instance (Show a) => Show (Circle a)

instance Shape Circle where
    area (Circle r) = pi*r**2 -- The culprit is here

--- | Polygon, similar idea here
data Polygon   a = (Fractional a) => Polygon { vertexes :: [(a,a)] } 
deriving instance (Show a) => Show (Polygon a)

instance Shape Polygon where
    area (Polygon vs) = polyarea vs
        where
        det (x1, y1) (x2, y2) = x1*y2 - x2*y1
        polyarea []       = 0
        polyarea [_]      = 0
        polyarea ps@(f:_) = 
            abs . (*) (1/2) . sum . mapDet $ ps 
            where 
            mapDet [p1]    = [det p1 f] 
            mapDet (p1:p2:pt) = det p1 p2 : (mapDet $ p2:pt)  

--- | POLYMORPHIC LISTS | ---
-----------------------------

-- If we want to get the areas of a bunch of different shapes we need to be able to 
-- store them in a list and they must all agree on what type their area function returns
totalArea :: (Floating a) => [SB a] -> a
totalArea = sum . map area  

--- | The Shape Box container datatype. Notice the triple class constraint
data SB a = forall s. (Num a, Show (s a), Shape s) => SB (s a) 

instance Shape SB where
    area (SB s) = area s

--- | We can show all the shapes in a list of SB's now
instance (Show a) => Show (SB a) where
    show (SB s) = show s

shapesList :: [SB Double]
shapesList = [ SB $ Rectangle { height = sqrt pi, width = sqrt pi}
             , SB $ Circle { radius = 1 }
             , SB $ Polygon { vertexes = [(0,1), (1/2,sqrt(3)/2), (sqrt(2)/2,sqrt(2)/2), (sqrt(3)/2,1/2),
                          (1,0), (sqrt(3)/2,-1/2), (sqrt(2)/2,-sqrt(2)/2), (1/2,-sqrt(3)/2),
                          (0,-1), (-sqrt(3)/2, -1/2), (-sqrt(2)/2,-sqrt(2)/2), (-sqrt(3)/2,-1/2),
                          (-1,0), (-sqrt(3)/2, 1/2), (-sqrt(2)/2,sqrt(2)/2), (-1/2,sqrt(3)/2)] }
             ]

--- Try:
-- | ghci> map area shapesList
-- | ghci> shapesList
-- | ghci> totalArea shapesList



