module PrettyLazyList exposing (..)

import Lazy exposing (Lazy, lazy, force)

type LazyList a
  = Nil
  | Cons a (Lazy (LazyList a))

head : LazyList a -> a
head l = case l of Cons x _ -> x
                   Nil      -> Debug.crash "head"

tail : LazyList a -> LazyList a
tail l = case l of Cons _ xs -> force xs
                   Nil       -> Debug.crash "tail"

range : Int -> Int -> LazyList Int
range i j =
  if i > j
    then Nil
    else Cons i (lazy (\() -> range (i+1) j))

infinite : Int -> LazyList Int
infinite i = Cons i (lazy (\_ -> infinite (i+1)))

take : Int -> LazyList a -> LazyList a
take k l = case (k, l) of
  (0, _)         -> Nil
  (_, Nil)       -> Nil
  (_, Cons x xs) -> Cons x (lazy (\() -> take (k-1) (force xs)))

drop : Int -> LazyList a -> LazyList a
drop k l = case (k, l) of
  (0, _)         -> l
  (_, Nil)       -> Nil
  (_, Cons _ xs) -> drop (k-1) (force xs)

append : LazyList a -> LazyList a -> LazyList a
append xs ys = case xs of
  Nil        -> ys
  Cons x xs_ -> Cons x (lazy (\_ -> append (force xs_) ys))

reverse : LazyList a -> LazyList a
reverse l =
  let foo acc xs = case xs of
    Nil        -> acc
    Cons x xs_ -> foo (Cons x (lazy (\_ -> acc))) (force xs_)
  in foo Nil l

toList : LazyList a -> List a
toList l =
  let foo acc l = case l of
    Nil       -> acc
    Cons x xs -> foo (x::acc) (force xs)
  in
  List.reverse <| foo [] l

eq : LazyList a -> LazyList a -> Bool
eq x y =
  let foo x y = case (x, y) of
    (Cons x xs, Cons y ys) ->
       if x == y
       then foo (force xs) (force ys)
       else False
    (Nil, Nil) -> True
    _          -> False
  in
  foo x y
