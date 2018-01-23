module BankersQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

import LazyList exposing (..)
import Lazy exposing (lazy, force)

nil = lazy (\_ -> Nil)

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

(+++) = append

----------------------------------------------------------------------

type Queue a = Q Int (LazyList a) Int (LazyList a)

empty : Queue a
empty = Q 0 nil 0 nil

isEmpty : Queue a -> Bool
isEmpty (Q i _ _ _) = i == 0

enqueue : a -> Queue a -> Queue a
enqueue x (Q i front j back) =
  check i front (j+1) (lazy (\_ -> Cons x back))

dequeue : Queue a -> Maybe (Queue a)
dequeue (Q i front j back) =
  if i == 0
    then Nothing
    else Just (check (i-1) (tail front) j back)

peek : Queue a -> Maybe a
peek (Q i front j back) =
  if i == 0
    then Nothing
    else Just (head front)

check i front j back =
  if j > i
    then Q (i+j) (front +++ reverse back) 0 nil
    else Q i front j back

------------------------------------------------------------------------------

enqueue_ x (Q i front j back) =
  if j < i
    then Q i front (j+1) (lazy (\_ -> Cons x back))
    else Q (i+j+1) (front +++ reverse back) 0 nil

dequeue_ (Q i front j back) =
  if i == 0 then Nothing
  else if i == j then Just (Q (i+j-1) (tail front +++ reverse back) 0 nil)
  else Just (Q (i-1) (tail front) j back)

