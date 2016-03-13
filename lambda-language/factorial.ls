let flip = \f x y -> f y x
let dec = flip - 1
let s = \f g x -> f x (g x)
let k = \x y   -> x
let b = \f g x -> f (g x)
let c = \f g x -> f x g
let y = \f -> f (y f)
let cond = \p f g x -> if (p x) (f x) (g x)
let fac = y (b (cond ((==) 0) (k 1)) (b (s (*)) (c b dec)))
