module EditDist where

data Edit = Change Char | Copy | Delete | Insert Char | Kill deriving (Show, Eq)

--- | Example «edit»
--- | ghci> edit [Delete, Copy, Copy, Copy, Copy] "ffish"
--- | "fish"
--- | ghci> edit [Insert 'c', Change 'h', Copy, Insert 'p', Copy, Kill] "fish"
--- | "chips"
--- | ghci> edit [Delete,Delete,Delete] "abcthingy"
--- | "thingy"
--- | You can also do silly things like
--- | ghci> flip edit "dips" . transform "fish" $ "chips" 
--- | "chipp"
--- | That just feeds the output of «transform» into edit, flip is used to swap the parameters around for edit

transform :: String -> String -> [Edit]
transform [] []         = []
transform _  []         = [Kill]
transform [] ys         = map Insert ys
transform (x:xs) (y:ys) = 
              if x == y then
                 Copy : transform xs ys
                else 
                 best [ Delete : transform xs (y:ys),
                        Change y : transform xs ys,
                        Insert y : transform (x:xs) ys]
              where
              best :: [[Edit]] -> [Edit]
              --- | Black magic
              best = snd . foldr1 (\(l2,x2) (l,x) -> if l <= l2 then (l,x) else (l2,x2)) . map (\x -> (length . filter (/= Copy) $ x, x))

--- | Good example of case of syntax using pattern matching
--- | syntax is:
--- | case <expression> of
--- |   pattern    -> value
--- |   pattern2   -> value2
--- |   _          ->         <== Wildcard, or default value
edit :: [Edit] -> String -> String
edit [] []        = []
edit [] xs        = xs
edit (e:es) []    = case e of
                        Insert c -> c : edit es []
                        Kill     -> []
                        _        -> edit es []
edit (e:es) (x:xs) = case e of
                        Copy     -> x : edit es xs
                        Delete   -> edit es xs
                        Change c -> c : edit es xs
                        Insert c -> c : edit es (x:xs)
                        Kill     -> []

