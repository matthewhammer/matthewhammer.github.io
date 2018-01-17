module Deque exposing
  (Deque, empty, isEmpty,
   addFront, removeFront, peekFront,
   addBack, removeBack, peekBack)

type Deque a = D { front : List a, back : List a }

mkD f b = D {front = f, back = b}

empty : Deque a
empty = mkD [] []

isEmpty : Deque a -> Bool
isEmpty q = q == empty

--------------------------------------------------------------------------------
-- FILL IN THE DEFINITIONS BELOW

check : List a -> List a -> Deque a
check f b =
  -- TODO
  empty

addFront : a -> Deque a -> Deque a
addFront x (D {front, back}) =
  -- TODO
  empty

addBack : a -> Deque a -> Deque a
addBack x (D {front, back}) =
  -- TODO
  empty

peekFront : Deque a -> Maybe a
peekFront (D {front, back}) =
  -- TODO
  Nothing

peekBack : Deque a -> Maybe a
peekBack (D {front, back}) =
  -- TODO
  Nothing

removeFront : Deque a -> Maybe (Deque a)
removeFront (D {front, back}) =
  -- TODO
  Nothing

removeBack : Deque a -> Maybe (Deque a)
removeBack (D {front, back}) =
  -- TODO
  Nothing

