module BHeaps exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

type Tree a = Node a (List (Tree a))
type alias Rank = Int
type Heap a = Heap (List (Rank, Tree a))

{-- Internal Helpers ----------------------------------------------------}

-- TODO

{-- External Interface --------------------------------------------------}

empty : Heap comparable
empty = WrapHeap []

isEmpty : Heap comparable -> Bool
isEmpty h = h == empty

insert : comparable -> Heap comparable -> Heap comparable
insert x h =
  -- TODO
  h

merge : Heap comparable -> Heap comparable -> Heap comparable
merge h1 h2 =
  -- TODO
  h1

findMin : Heap comparable -> Maybe comparable
findMin h =
  -- TODO
  Nothing

deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin h =
  -- TODO
  Nothing

