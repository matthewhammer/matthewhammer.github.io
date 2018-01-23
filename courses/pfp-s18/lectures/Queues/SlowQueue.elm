module SlowQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

type Queue a = Q (List a)

empty : Queue a
empty = Q []

isEmpty : Queue a -> Bool
isEmpty q = q == empty

enqueue : a -> Queue a -> Queue a      -- Theta(n)
enqueue x (Q xs) = Q (xs ++ [x])

dequeue : Queue a -> Maybe (Queue a)   -- O(1)
dequeue (Q xs) = case xs of
  _::xs_ -> Just (Q xs_)
  []     -> Nothing

peek : Queue a -> Maybe a              -- O(1)
peek (Q xs) = case xs of
  x::_   -> Just x
  []     -> Nothing

