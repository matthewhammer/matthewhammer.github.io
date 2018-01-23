module RealTimeQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

import LazyList exposing (..)
import Lazy exposing (Lazy, lazy, force)

maybeHead l = case force l of
  Cons x _ -> Just x
  Nil -> Nothing

maybeTail l = case force l of
  Cons _ xs -> Just xs
  Nil -> Nothing

fromJust mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash "fromJust"

head = fromJust << maybeHead
tail = fromJust << maybeTail

----------------------------------------------------------------------

type Queue a = Q (LazyList a) (List a) (LazyList a)

rotate : LazyList a -> List a -> LazyList a -> LazyList a
rotate xs ys acc =
  case (force xs, ys) of
    (Nil        , y::[])  -> lazy (\_ -> Cons y acc)
    (Cons x xs_ , y::ys_) ->
      lazy (\_ -> Cons x (rotate xs_ ys_ (lazy (\_ -> Cons y acc))))
    _ -> Debug.crash "rotate"

empty : Queue a
empty = Q (lazy (\_ -> Nil)) [] (lazy (\_ -> Nil))

isEmpty : Queue a -> Bool
isEmpty (Q front _ _) =
  case force front of
    Nil -> True
    _   -> False

peek : Queue a -> Maybe a
peek (Q front _ _) = maybeHead front

enqueue : a -> Queue a -> Queue a
enqueue x (Q front back sched) = exec front (x::back) sched

dequeue : Queue a -> Maybe (Queue a)
dequeue (Q front back sched) =
  case force front of
    Nil       -> Nothing
    Cons _ f_ -> Just (exec f_ back sched)

exec front back sched =
  case force sched of
    Cons _ sched_ -> Q front back sched_
    Nil ->
      let front_ = rotate front back (lazy (\_ -> Nil)) in
      Q front_ [] front_

------------------------------------------------------------------------------

enqueue_ : a -> Queue a -> Queue a
enqueue_ x (Q front back sched) =
  case force sched of
    Cons _ sched_ -> Q front (x::back) sched_
    Nil ->
      let front_ = rotate front (x::back) (lazy (\_ -> Nil)) in
      Q front_ [] front_

dequeue_ : Queue a -> Maybe (Queue a)
dequeue_ (Q front back sched) =
  case force front of
    Nil -> Nothing
    Cons _ front_ ->
      case force sched of
        Cons _ sched_ -> Just (Q front_ back sched_)
        Nil ->
          let front__ = rotate front_ back (lazy (\_ -> Nil)) in
          Just (Q front__ [] front__)

