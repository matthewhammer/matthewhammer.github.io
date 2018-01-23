module LeftistHeap exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

type alias Rank = Int

type Heap a = E | T Rank a (Heap a) (Heap a)

rank : Heap a -> Rank
rank h =
  case h of
    E         -> 0
    T r _ _ _ -> r

makeT : a -> Heap a -> Heap a -> Heap a
makeT x h1 h2 =
  let (left,right) =
    if rank h1 >= rank h2
      then (h1, h2)
      else (h2, h1)
  in
  T (1 + rank right) x left right

merge : Heap comparable -> Heap comparable -> Heap comparable
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2 
  (T _ x1 left1 right1, T _ x2 left2 right2) ->
    if x1 <= x2
      then makeT x1 left1 (merge right1 h2)
      else makeT x2 left2 (merge h1 right2)

insert : comparable -> Heap comparable -> Heap comparable
insert x h = merge (T 1 x E E) h

deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin h =
  case h of
    E         -> Nothing
    T _ x a b -> Just (x, merge a b)

findMin : Heap comparable -> Maybe comparable
findMin h =
  case h of
    E         -> Nothing
    T _ x _ _ -> Just x

empty : Heap comparable
empty = E

isEmpty : Heap comparable -> Bool
isEmpty h = h == empty

------------------------------------------------------------------------------

{-

computeRank : Heap a -> Rank
computeRank h =
  case h of
    E -> 0
    T r _ left right ->
      if r == (1 + computeRank right)
        then r
        else Debug.crash "incorrect rank"

size : Heap a -> Int
size h =
  case h of
    E         -> 0
    T _ _ a b -> 1 + size a + size b

value : Heap a -> Maybe a
value h =
  case h of
    E         -> Nothing
    T _ x _ _ -> Just x

left : Heap a -> Heap a
left h =
  case h of
    E         -> E
    T _ _ a _ -> a

right : Heap a -> Heap a
right h =
  case h of
    E         -> E 
    T _ _ _ b -> b

log = logBase 2

-}
