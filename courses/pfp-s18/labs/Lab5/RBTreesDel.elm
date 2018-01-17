module RBTreesDel exposing (Set, empty, member, insert, delete, size)

import RBMaps as Map

type Set comparable = Set (Map.Map comparable Bool)

empty : Set comparable
empty =
  Debug.crash "TODO"

member : comparable -> Set comparable -> Bool
member =
  Debug.crash "TODO"

insert : comparable -> Set comparable -> Set comparable
insert =
  Debug.crash "TODO"

delete : comparable -> Set comparable -> Set comparable
delete =
  Debug.crash "TODO"

size : Set comparable -> (Int, Int)
size =
  Debug.crash "TODO"

