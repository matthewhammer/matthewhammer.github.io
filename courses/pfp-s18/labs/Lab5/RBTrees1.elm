module RBTrees1 exposing (..)

import RedBlackTree exposing (Color(..), Tree(..))

insert : comparable -> Tree comparable -> Tree comparable
insert x t =
  case ins x t of
    T _ l y r -> T B l y r
    E         -> Debug.crash "insert"

ins : comparable -> Tree comparable -> Tree comparable
ins =
  Debug.crash "TODO"

type alias Balance comparable =
  Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable

balanceL : Balance comparable
balanceL =
  Debug.crash "TODO"

balanceR : Balance comparable
balanceR =
  Debug.crash "TODO"

