module RBMaps exposing (Map, empty, get, insert, toList)

type Color = R | B
type Map comparable v
   = E
   | T Color (Map comparable v) (comparable, v) (Map comparable v)

empty : Map comparable v
empty =
  Debug.crash "TODO"

get : comparable -> Map comparable v -> Maybe v
get =
  Debug.crash "TODO"

insert : comparable -> v -> Map comparable v -> Map comparable v
insert =
  Debug.crash "TODO"

ins : comparable -> v -> Map comparable v -> Map comparable v
ins =
  Debug.crash "TODO"

balance : Color
       -> Map comparable v -> (comparable, v) -> Map comparable v
       -> Map comparable v
balance =
  Debug.crash "TODO"

toList : Map comparable v -> List (comparable, v)
toList =
  Debug.crash "TODO"

