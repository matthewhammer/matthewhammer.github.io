module EnumerateRB exposing (..)

import LazyList exposing (..)
import LazyListPlus
import Lazy exposing (Lazy, lazy, force)

------------------------------------------------------------------------------

type Color  = R | B
type Tree a = E | T Color (Tree a) a (Tree a)

check bh t =
  noRedRed t && blackHeight t == Just bh

color t = case t of
  T c _ _ _ -> c
  E         -> B

root t = case t of
  T _ _ x _ -> x
  E         -> Debug.crash "root"

blackHeight t = case t of
  E -> Just 0
  T c l _ r ->
    blackHeight l |> Maybe.andThen (\n ->
    blackHeight r |> Maybe.andThen (\m ->
      if n /= m then Nothing
      else if c == B then Just (1 + n)
      else Just n
    ))

okBlackHeight t = case blackHeight t of
  Just _  -> True
  Nothing -> False

noRedRed t = case t of
  E                   -> True
  T R (T R _ _ _) _ _ -> False
  T R _ _ (T R _ _ _) -> False
  T _ l _ r           -> List.all noRedRed [l, r]

------------------------------------------------------------------------------

listRBTrees : Int -> List (Tree ())
listRBTrees bh =
  Debug.crash "TODO"

------------------------------------------------------------------------------

lazyListRBTrees : Int -> LazyList (Tree ())
lazyListRBTrees bh =
  Debug.crash "TODO"
