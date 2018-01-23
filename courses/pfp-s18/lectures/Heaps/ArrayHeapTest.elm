module ArrayHeapTest exposing (..)

import ArrayHeap exposing (Heap)

insertAll : List comparable -> Heap comparable
insertAll =
  List.foldl ArrayHeap.insert ArrayHeap.empty

extractAll : Heap comparable -> List comparable
extractAll heap =
  case ArrayHeap.deleteMin heap of
    Nothing        -> []
    Just (x, rest) -> x :: extractAll rest

simpleHeapSort : List comparable -> List comparable
simpleHeapSort =
  extractAll << insertAll

test : List comparable -> Bool
test list =
  simpleHeapSort list == List.sort list
