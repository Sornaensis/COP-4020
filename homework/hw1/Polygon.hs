module Polygon (
                det,
                computePolygonArea
               ) where

-- | Author:    Kyle Jones
-- | Date:      2016-02-10
-- | Course:    COP-4020

import Prelude hiding (($),(.)) -- HAPPY FUNTIMES TO ENSUE

---  | FOR MAKE BETTERER USAGE IN HASKELL | ---
infixl 9 .
infixl 0 $
-- | flip the fixity and arguments to do everything in »REVERSE«   MUUUAHAHAHAH
($) :: a -> (a -> b) -> b
x $ f = f x
(.) :: (a -> b) -> (b -> c) -> a -> c
(f . g) x = g (f x)
--------------------------------------------- |

det :: (Double, Double) -> (Double, Double) -> Double
det (x1, y1) (x2, y2) = x1*y2 - x2*y1

computePolygonArea :: [(Double, Double)] -> Double
computePolygonArea []       = error "empty list"
computePolygonArea [_]      = error "Single element list"
computePolygonArea ps@(f:_) = 
-- | Pass data forward, like a /bin/sh pipeline -->
     ps $ mapDet . sum . (* (1/2)) . abs
     where 
     mapDet [p1]    = [det p1 f] 
     mapDet (p1:p2:pt) = p2:pt $ mapDet . ( det p1 p2 $ (:) ) -- Yea this is just to annoy anyone trying to read the code
