module FastQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

type Queue a = Q { front : List a, back : List a }

mkQ f b = Q {front = f, back = b}

empty : Queue a
empty = mkQ [] []

isEmpty : Queue a -> Bool
isEmpty q = q == empty

enqueue : a -> Queue a -> Queue a      -- O(1)
enqueue x (Q {front, back}) =
  case front of
    [] -> mkQ [x] []
    _  -> mkQ front (x::back)

dequeue : Queue a -> Maybe (Queue a)   -- O(n)
dequeue (Q {front, back}) =
  case front of
    []    -> Nothing
    _::[] -> Just (mkQ (List.reverse back) [])
    _::f_ -> Just (mkQ f_ back)

peek : Queue a -> Maybe a              -- O(1)
peek (Q {front, back}) = List.head front

------------------------------------------------------------------------------

checkFront : List a -> List a -> Queue a
checkFront f b =
  case f of
    [] -> mkQ (List.reverse b) []
    _  -> mkQ f b

enqueue_ : a -> Queue a -> Queue a      -- O(1)
enqueue_ x (Q {front, back}) = checkFront front (x::back)

dequeue_ : Queue a -> Maybe (Queue a)   -- O(n)
dequeue_ (Q {front, back}) =
  case front of
    []    -> Nothing
    _::f_ -> Just (checkFront f_ back)

