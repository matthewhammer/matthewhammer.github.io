module BinomialHeap exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

type alias Rank = Int
type Tree a = Node Rank a (List (Tree a))

type Heap a = Heap (List (Tree a))

rank (Node r _ _) = r
root (Node _ x _) = x

link : Tree comparable -> Tree comparable -> Tree comparable
link t1 t2 =
  let
    (Node r x1 ts1) = t1
    (Node _ x2 ts2) = t2
  in
  if x1 <= x2
    then Node (r+1) x1 (t2::ts1)
    else Node (r+1) x2 (t1::ts2)

insertTree : Tree comparable -> List (Tree comparable) -> List (Tree comparable)
insertTree t ts = case ts of
  []      -> [t]
  t1::ts1 ->
    if rank t == rank t1 then insertTree (link t t1) ts1
    else if rank t < rank t1 then t :: ts
    else Debug.crash "insertTree: impossible"

merge_
    : List (Tree comparable) -> List (Tree comparable)
   -> List (Tree comparable)
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1_rest, t2::ts2_rest) ->
    if rank t1 < rank t2 then t1 :: merge_ ts1_rest ts2
    else if rank t2 < rank t1 then t2 :: merge_ ts2_rest ts1
    else insertTree (link t1 t2) (merge_ ts1_rest ts2_rest)

removeMinTree
    : List (Tree comparable)
   -> (Tree comparable, List (Tree comparable))
removeMinTree ts = case ts of
  []     -> Debug.crash "removeMinTree: impossible"
  [t]    -> (t, [])
  t::ts_rest ->
    let (minTree, restTrees) = removeMinTree ts_rest in
    if root t < root minTree
      then (t, ts_rest)
      else (minTree, t::restTrees)

empty : Heap comparable
empty = Heap []

isEmpty : Heap comparable -> Bool
isEmpty h = h == empty

insert : comparable -> Heap comparable -> Heap comparable
insert x (Heap ts) = Heap (insertTree (Node 0 x []) ts)

merge : Heap comparable -> Heap comparable -> Heap comparable
merge (Heap ts1) (Heap ts2) = Heap (merge_ ts1 ts2)

findMin0 : Heap comparable -> Maybe comparable
findMin0 (Heap ts) =
  case List.map root ts of
    []    -> Nothing
    n::ns -> Just (List.foldl min n ns)

findMin : Heap comparable -> Maybe comparable
findMin (Heap ts) =
  case ts of
    [] -> Nothing
    _  -> Just (root (Tuple.first (removeMinTree ts)))

deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin (Heap ts) = case ts of
  [] -> Nothing
  _  -> let (Node _ x ts1, ts2) = removeMinTree ts in
        Just (x, Heap (merge_ (List.reverse ts1) ts2))


-- Alternative definition of merge_, adapted from:
--
-- http://stackoverflow.com/questions/11462626/
--   should-melding-merging-of-binomial-heaps-be-done-in-one-pass-or-two

merge_one_pass
    : List (Tree comparable) -> List (Tree comparable)
   -> List (Tree comparable)
merge_one_pass ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1_rest, t2::ts2_rest) ->
    if rank t1 < rank t2 then t1 :: merge_one_pass ts1_rest ts2
    else if rank t2 < rank t1 then t2 :: merge_one_pass ts1 ts2_rest
    else merge_wc (link t1 t2) ts1_rest ts2_rest

merge_wc
    : Tree comparable -> List (Tree comparable) -> List (Tree comparable)
   -> List (Tree comparable)
merge_wc t ts1 ts2 = case (ts1, ts2) of
  ([], _) -> insertTree t ts2
  (_, []) -> insertTree t ts1
  (t1::ts1_rest, t2::ts2_rest) ->
    let (r,r1,r2) = (rank t, rank t1, rank t2) in
    if r <  r1 && r <  r2 then t :: merge_one_pass ts1 ts2
    else if r <  r1 && r == r2 then merge_wc (link t t2) ts1 ts2_rest
    else if r == r1 && r <  r2 then merge_wc (link t t1) ts1_rest ts2
    else if r == r1 && r == r2 then t :: merge_wc (link t1 t2) ts1_rest ts2_rest
    -- else if r == r1 && r == r2 then merge_wc (link t t1) ts1_rest ts2
    else Debug.crash "merge_wc: impossible"
