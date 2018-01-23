module NotSoLazyList exposing (..)

import Lazy exposing (Lazy, lazy, force)

type LazyList a
  = Nil
  | Cons (Lazy a) (LazyList a)

range : Int -> Int -> LazyList Int
range i j =
  let foo acc n =
    if n > j
      then acc
      else foo (Cons (lazy (\_ -> n)) acc) (n+1)
  in
  foo Nil i

eq : LazyList a -> LazyList a -> Bool
eq x y = case (x, y) of
  (Nil, Nil)             -> True
  (Cons x xs, Cons y ys) -> (force x == force y) && (eq xs ys)
  _                      -> False

