module LazyList exposing (..)

import Lazy exposing (Lazy, lazy, force)

type alias LazyList a = Lazy (LazyListCell a)

type LazyListCell a
  = Nil
  | Cons a (LazyList a)

nil         = lazy (\_ -> Nil)
singleton x = lazy (\_ -> Cons x nil)

toList : LazyList a -> List a
toList xs =
  let foo acc xs = case force xs of
    Nil        -> acc
    Cons x xs_ -> foo (x::acc) xs_
  in
  List.reverse <| foo [] xs

range : Int -> Int -> LazyList Int
range i j =
  if i > j
    then lazy (\_ -> Nil)
    else lazy (\_ -> Cons i (range (i+1) j))

range_ : Int -> Int -> LazyList Int
range_ i j =
  if i > j then lazy (\_ -> Nil)
  else lazy <| \_ ->
    let _ = Debug.log "force" i in
    Cons i (range_ (i+1) j)

infinite : Int -> LazyList Int
infinite i = lazy (\_ -> Cons i (infinite (i+1)))

infinite_ : Int -> LazyList Int
infinite_ i = lazy <| \_ ->
  let _ = Debug.log "force" i in
  Cons i (infinite_ (i+1))

take : Int -> LazyList a -> LazyList a
take k xs =
  if k <= 0 then lazy (\_ -> Nil)
  else
    lazy <| \_ ->
      case force xs of
        Nil        -> Nil
        Cons x xs_ -> Cons x (take (k-1) xs_)

drop : Int -> LazyList a -> LazyList a
drop k xs =
  if k <= 0 then xs
  else
    lazy <| \_ ->
      case force xs of
        Nil        -> Nil
        Cons _ xs_ -> force (drop (k-1) xs_)

append : LazyList a -> LazyList a -> LazyList a
append xs ys =
  lazy <| \_ ->
    case force xs of
      Nil        -> force ys
      Cons x xs_ -> Cons x (append xs_ ys)

reverse : LazyList a -> LazyList a
reverse = reverse1

reverse0 : LazyList a -> LazyList a
reverse0 xs =
  lazy <| \_ ->
    case force xs of
      Nil        -> Nil
      Cons x xs_ -> force (append (reverse0 xs_) (singleton x))

reverse1 : LazyList a -> LazyList a
reverse1 xs =
  let foo acc xs =
    case force xs of
      Nil        -> acc
      Cons x xs_ -> lazy (\_ -> force (foo (lazy (\_ -> Cons x acc)) xs_))
   -- Cons x xs_ -> foo (lazy (\_ -> Cons x acc)) xs_
  in
  lazy (\_ -> force (foo nil xs))

reverse2 : LazyList a -> LazyList a
reverse2 xs =
  let foo acc xs =
    case force xs of
      Nil        -> acc
      Cons x xs_ -> foo (lazy (\_ -> Cons x acc)) xs_
  in
  foo nil xs

eq : LazyList a -> LazyList a -> Bool
eq xs ys =
  case (force xs, force ys) of
    (Nil, Nil)               -> True
    (Cons x xs_, Cons y ys_) -> if x /= y then False else eq xs_ ys_
    _                        -> False
