module MediumQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

type Queue a = Q { front: List a, back: List a }

mkQ f b = Q {front = f, back = b}

empty : Queue a
empty = mkQ [] []

isEmpty : Queue a -> Bool
isEmpty q = q == empty

enqueue : a -> Queue a -> Queue a      -- O(1)
enqueue x (Q {front, back}) = mkQ front (x::back)

dequeue : Queue a -> Maybe (Queue a)   -- O(n)
dequeue (Q {front, back}) = case (front, back) of
  (_::f_, _) -> Just (mkQ f_ back) 
  ([], [])   -> Nothing
  ([], _)    -> dequeue (mkQ (List.reverse back) [])

peek : Queue a -> Maybe a              -- O(n)
peek (Q {front, back}) = case (front, back) of
  (x::_, _)  -> Just x
  ([], [])   -> Nothing
  ([], _)    -> peek (mkQ (List.reverse back) [])

