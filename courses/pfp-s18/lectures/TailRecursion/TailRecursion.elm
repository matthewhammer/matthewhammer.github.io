module TailRecursion exposing (..)

------------------------------------------------------------------------------

sum n =
  if n <= 0
    then 0
    else n + sum (n-1)

sum_tr_ : Int -> Int -> Int
sum_tr_ acc n =
  if n <= 0
    then acc
    else sum_tr_ (n+acc) (n-1)

sum_tr = sum_tr_ 0

------------------------------------------------------------------------------

foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc xs = case xs of
  []     -> acc
  x::xs_ -> foldl f (f x acc) xs_

foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc xs = case xs of
  []     -> acc
  x::xs_ -> f x (foldr f acc xs_)
