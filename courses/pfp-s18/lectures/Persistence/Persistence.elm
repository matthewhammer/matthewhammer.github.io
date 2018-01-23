module Persistence exposing (..)

find x xs = case xs of
  []      -> False
  y::rest -> if x == y then True else find x rest

update : List a -> Int -> a -> List a
update xs i y =
  case (xs, i) of
    ([],      _) -> []
    (x::rest, 0) -> y :: rest
    (x::rest, _) -> x :: update rest (i-1) y

append : List a -> List a -> List a
append xs ys =
  case xs of
    []      -> ys
    x::rest -> x :: append rest ys

type Tree a = Empty | Node a (Tree a) (Tree a)

findBT : a -> Tree a -> Bool
findBT x t = case t of
  Empty             -> False
  Node y left right -> x == y || findBT x left || findBT x right

findBST : comparable -> Tree comparable -> Bool
findBST x t = case t of
  Empty -> False
  Node y left right ->
    if x == y then      True
    else if x < y then  findBST x left
    else {- x > y -}    findBST x right

insert : comparable -> Tree comparable -> Tree comparable
insert x t = case t of
  Empty -> Node x Empty Empty
  Node y left right ->
    if x == y then      t
    else if x < y then  Node y (insert x left) right
    else {- x > y -}    Node y left (insert x right)

