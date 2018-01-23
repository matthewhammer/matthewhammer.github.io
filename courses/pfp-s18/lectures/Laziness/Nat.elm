module Nat exposing (..)

type Nat = Z | S Nat

-- fromInt n =
--   if n <= 0
--     then Z
--     else S (fromInt (n-1))

fromInt n =
  let foo acc n =
    if n <= 0
      then acc
      else foo (S acc) (n-1)
  in foo Z n

foldl : (b -> b) -> b -> Nat -> b
foldl f acc n =
  case n of
    Z    -> acc
    S n_ -> foldl f (f acc) n_

toInt : Nat -> Int
toInt = foldl ((+) 1) 0

strNat : Nat -> String
strNat = foldl ((++) "S") "Z"

plus : Nat -> Nat -> Nat
plus = foldl S

eqNat : Nat -> Nat -> Bool
eqNat x y =
  let foo x y = case (x, y) of
    (Z, Z)       -> True
    (S x_, S y_) -> foo x_ y_
    _            -> False
  in foo x y
