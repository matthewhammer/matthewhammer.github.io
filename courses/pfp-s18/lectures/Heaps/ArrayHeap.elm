module ArrayHeap exposing
  (Heap, empty, isEmpty, findMin, deleteMin, insert, merge)

import Array exposing (Array)


type Heap a = Heap (Array a)


leftIdx i   = 2*i + 1
rightIdx i  = 2*i + 2
parentIdx i = (i-1) // 2


addToEnd : a -> Array a -> Array a
addToEnd = Array.push


removeFromEnd : Array a -> Maybe (a, Array a)
removeFromEnd array =
  let n = Array.length array in
  case Array.get (n-1) array of
    Nothing   -> Nothing
    Just last -> Just (last, Array.slice 0 -1 array)


justGet : Int -> Array a -> a
justGet i array =
  case Array.get i array of
    Nothing -> Debug.crash ("justGet: " ++ toString i)
    Just x  -> x


swap : Int -> Int -> Array a -> Array a
swap i j array =
  let ai = justGet i array in
  let aj = justGet j array in
  array |> Array.set i aj |> Array.set j ai


empty : Heap comparable
empty = Heap Array.empty


isEmpty : Heap comparable -> Bool
isEmpty (Heap array) =
  Array.isEmpty array


findMin : Heap comparable -> Maybe comparable
findMin (Heap array) =
  Array.get 0 array


insert : comparable -> Heap comparable -> Heap comparable
insert x (Heap array) =
  array
    |> addToEnd x
    |> bubbleUp (Array.length array)
    |> Heap


deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin (Heap array) =
  case removeFromEnd array of
    Nothing -> Nothing
    Just (lastElement, choppedArray) ->
      let minElement = justGet 0 array in
      let newArray =
        choppedArray
          |> Array.set 0 lastElement
          |> bubbleDown 0
      in
      Just (minElement, Heap newArray)


bubbleUp : Int -> Array comparable -> Array comparable
bubbleUp i array =
  let
    child  = justGet i array
    parent = justGet (parentIdx i) array
  in
  if parent <= child
    then array
    else array |> swap i (parentIdx i) |> bubbleUp (parentIdx i)


bubbleDown : Int -> Array comparable -> Array comparable
bubbleDown i array =
  let n = Array.length array in
  let smaller j acc =
    if j < n && justGet j array < justGet acc array
      then j
      else acc
  in
  let smallest = i |> smaller (leftIdx i) |> smaller (rightIdx i) in
  if i == smallest
    then array
    else array |> swap i smallest |> bubbleDown smallest

{-

Verbose Implementation:

bubbleDown i array =

  let swapAndRecurse j = array |> swap i j |> bubbleDown j in
  let bubbleDownLeft () = swapAndRecurse (leftIdx i) in
  let bubbleDownRight () = swapAndRecurse (rightIdx i) in

  if leftIdx i >= Array.length array then
    array

  else if rightIdx i >= Array.length array then 
    let
      this  = justGet i array
      left  = justGet (leftIdx i) array
    in
    if this <= left
      then array
      else bubbleDownLeft ()

  else
    let
      this  = justGet i array
      left  = justGet (leftIdx i) array
      right = justGet (rightIdx i) array
    in
    if this <= left && this <= right then array
    else if left < this && this <= right then bubbleDownLeft ()
    else if right < this && this <= left then bubbleDownRight ()
    else {- left <= this && right <= this -}
      if left <= right
        then bubbleDownLeft ()
        else bubbleDownRight ()
-}


merge : Heap comparable -> Heap comparable -> Heap comparable
merge heap1 heap2 =
  Debug.crash "merge: not implemented"
