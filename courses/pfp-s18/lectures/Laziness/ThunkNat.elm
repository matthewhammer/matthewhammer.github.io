module ThunkNat exposing (..)

type Nat = Z | S (Thunk Nat)

type alias Thunk a = () -> a

force : Thunk a -> a
force thunk = thunk ()

fromInt : Int -> Nat
fromInt n =
  if n <= 0
    then Z
    else S (\_ -> fromInt (n-1))

toInt : Nat -> Int
toInt n =
  let foo acc n = case n of
    Z    -> acc
    S n_ -> foo (1 + acc) (force n_)
  in foo 0 n

strNat : Nat -> String
strNat n =
  let foo acc n = case n of
    Z    -> acc
    S n_ -> foo ("S" ++ acc) (force n_)
  in foo "Z" n

eqNat : Nat -> Nat -> Bool
eqNat x y =
  let foo x y = case (x, y) of
    (Z, Z)       -> True
    (S x_, S y_) -> foo (force x_) (force y_)
    (_, _)       -> False
  in foo x y

plus : Nat -> Nat -> Nat
plus x y = case y of
  Z    -> x
  S y_ -> S (\_ -> plus x (force y_))
