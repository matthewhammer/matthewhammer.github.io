module AnotherSlowQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

type Queue a = Front (List a) | Back (List a)

empty : Queue a
empty = Front []

isEmpty : Queue a -> Bool
isEmpty q = case q of
  Front [] -> True
  Back []  -> True
  _        -> False

enqueue : a -> Queue a -> Queue a      -- O(n)
enqueue x q = case q of
  Front xs -> Back (x :: List.reverse xs)
  Back xs  -> Back (x :: xs)

dequeue : Queue a -> Maybe (Queue a)   -- O(n)
dequeue q = case q of
  Back []      -> Nothing
  Back (x::xs) -> Just (Back xs)
  Front xs     -> dequeue (Back (List.reverse xs))

peek : Queue a -> Maybe a              -- O(n)
peek q = case q of
  Back []      -> Nothing
  Back (x::_)  -> Just x
  Front xs     -> peek (Back (List.reverse xs))

