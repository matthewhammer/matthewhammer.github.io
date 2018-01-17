module ExplicitMin exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

import BinomialHeap as H
-- import LeftistHeap as H

type Heap
  = E
  | NE a (H.Heap a)   -- the a is the minimum element

empty : Heap comparable
empty = E

isEmpty : Heap comparable -> Bool
isEmpty _ =
  -- TODO
  False

insert : comparable -> Heap comparable -> Heap comparable
insert _ _ =
  -- TODO
  E

merge : Heap comparable -> Heap comparable -> Heap comparable
merge _ _ =
  -- TODO
  E

findMin : Heap comparable -> Maybe comparable
findMin _ =
  -- TODO
  Nothing

deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin _ =
  -- TODO
  Nothing

