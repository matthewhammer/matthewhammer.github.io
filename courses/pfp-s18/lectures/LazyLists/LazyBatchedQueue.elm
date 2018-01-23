module LazyBatchedQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

import LazyList exposing (..)
import Lazy exposing (Lazy, lazy, force)

type Queue a = Q { front : LazyList a, back : LazyList a }

mkQ f b = Q {front = f, back = b}

nil = lazy (\_ -> Nil)

empty : Queue a
empty = mkQ nil nil

isEmpty : Queue a -> Bool
isEmpty (Q {front, back}) =
  case force front of
    Nil -> True
    _   -> False

enqueue : a -> Queue a -> Queue a
enqueue x (Q {front, back}) = checkFront front (lazy (\_ -> Cons x back))

dequeue : Queue a -> Maybe (Queue a)
dequeue (Q {front, back}) = case force front of
  Nil       -> Nothing
  Cons _ f_ -> Just (checkFront f_ back)

peek : Queue a -> Maybe a
peek (Q {front, back}) = case force front of
  Nil      -> Nothing
  Cons x _ -> Just x

checkFront : LazyList a -> LazyList a -> Queue a
checkFront f b = case force f of
  Nil -> mkQ (lazy (\_ -> force (reverse b))) nil
  _   -> mkQ f b

