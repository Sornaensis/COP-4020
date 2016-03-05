module SimpleFunctions where

merge :: Ord a => ([a],[a]) -> [a]
merge ([], ys)               = ys
merge (xs, [])               = xs
merge (xt@(x:xs), yt@(y:ys)) = if x < y then x : merge (xs, yt) else y : merge (xt, ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ys = let  l = length ys `div` 2 
                    ls = mergeSort $ drop l ys
                    fs = mergeSort $ take l ys
               in merge (fs, ls)

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst f (x:xs) | not $ f x = xs
                     | otherwise = x : filterFirst f xs

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f = reverse . filterFirst f . reverse 

split :: [a] -> ([a],[a])
split xs =  (splitL xs, splitR xs)
            where
            splitL []       = []
            splitL [x]      = [x]
            splitL (x:_:xs) = x : splitL xs
            splitR []       = []
            splitR [_]      = []
            splitR (_:y:ys) = y : splitR ys

interleave :: ([a],[a]) -> [a]
interleave (xs, ys) = combine xs ys
                      where 
                      combine [] ys            = ys
                      combine xs []            = xs
                      combine (x:xs) (y:ys)    = x : y : combine xs ys
