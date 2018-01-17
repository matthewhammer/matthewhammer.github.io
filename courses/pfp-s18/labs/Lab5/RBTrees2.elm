module RBTrees2 exposing (..)

import RedBlackTree exposing (Color(..), Tree(..))

insert : comparable -> Tree comparable -> Tree comparable
insert =
  Debug.crash "TODO"

type Dir = Left | Right

ins : comparable -> Tree comparable -> (Tree comparable, List Dir)
ins =
  Debug.crash "TODO"

type alias Balance comparable =
  Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable

chooseBalance : List Dir -> Balance comparable
chooseBalance =
  Debug.crash "TODO"

balanceLL : Balance comparable
balanceLL =
  Debug.crash "TODO"

balanceLR : Balance comparable
balanceLR =
  Debug.crash "TODO"

balanceRL : Balance comparable
balanceRL =
  Debug.crash "TODO"

balanceRR : Balance comparable
balanceRR =
  Debug.crash "TODO"

