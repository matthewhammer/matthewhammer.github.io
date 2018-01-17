module LazyListPlus exposing (..)

import LazyList exposing (..)
import Lazy exposing (Lazy, lazy, force)

map : (a -> b) -> LazyList a -> LazyList b
map f xs =
  Debug.crash "TODO"

concat : LazyList (LazyList a) -> LazyList a
concat xss =
  Debug.crash "TODO"

concatMap : (a -> LazyList b) -> LazyList a -> LazyList b
concatMap f xs =
  Debug.crash "TODO"

cartProdWith : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
cartProdWith f xs ys =
  Debug.crash "TODO"
