module ListsAndTrees exposing
  ( suffixes
  , mem, fullTree, balancedTree, balancedTrees
  , completeTrees, almostCompleteTrees
  )


------------------------------------------------------------------------------
-- Problem 1 
------------------------------------------------------------------------------

suffixes : List a -> List (List a)
suffixes xs =
  -- TODO
  []


------------------------------------------------------------------------------
-- Problem 2 
------------------------------------------------------------------------------

type Tree a = Empty | Node a (Tree a) (Tree a)

mem : comparable -> Tree comparable -> Bool
mem _ _ =
  -- TODO
  False

fullTree : a -> Int -> Tree a
fullTree _ _ =
  -- TODO
  Empty

balancedTree : a -> Int -> Tree a
balancedTree _ _ =
  -- TODO
  Empty

create2 : a -> Int -> (Tree a, Tree a)
create2 _ _ =
  -- TODO
  (Empty, Empty)

balancedTrees : a -> Int -> List (Tree a)
balancedTrees _ _ =
  -- TODO
  []

completeTrees : a -> Int -> List (Tree a)
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : a -> Int -> List (Tree a)
almostCompleteTrees _ _ =
  -- TODO
  []

