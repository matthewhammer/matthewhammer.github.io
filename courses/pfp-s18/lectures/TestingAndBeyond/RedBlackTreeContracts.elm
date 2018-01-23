module RedBlackTreeContracts exposing (..)

import Contracts

maybeMonitor = Contracts.makeMaybeMonitors True
    -- toggle boolean flag to turn contracts on/off

------------------------------------------------------------------------------

type Color  = R | B
type Tree a = E | T Color (Tree a) a (Tree a)

empty = E

member : comparable -> Tree comparable -> Bool
member x t = case t of
  E -> False
  T _ l y r ->
    if x == y then True
    else if x < y then member x l
    else member x r

insert : comparable -> Tree comparable -> Tree comparable
insert =
  maybeMonitor.two "insert"
      (\x -> True)
      (\_ t -> rb t)
      (\_ t ret -> rb ret && (bh ret == bh t || bh ret == 1 + bh t)) <|
  \x t ->
    case ins x t of
      T _ l y r -> T B l y r
      E         -> Debug.crash "insert"

ins : comparable -> Tree comparable -> Tree comparable
ins =
  maybeMonitor.two "ins"
      (\x -> True)
      (\_ t -> rb t)
      (\_ t ret -> rbExceptMaybeRoot ret && bh t == bh ret) <|
  \x t ->
    case t of
      E -> T R E x E
      T c l y r ->
        if x == y then t
        else if x < y then balance c (ins x l) y r
        else balance c l y (ins x r)

balance : Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable
balance =
  maybeMonitor.four "balance"
      (\c -> True)
      (\c l -> rbExceptMaybeRoot l)
      (\c l val -> True)
      (\c l val r -> if oneRedRed l then rb r else rbExceptMaybeRoot r)
      (\c l val r ret -> if oneRedRed (T c l val r) then rbExceptMaybeRoot ret else rb ret) <|
  \c l val r ->
    case (c, l, val, r) of
      (B, T R (T R a x b) y c, z, d) -> T R (T B a x b) y (T B c z d)
      (B, T R a x (T R b y c), z, d) -> T R (T B a x b) y (T B c z d)
      (B, a, x, T R (T R b y c) z d) -> T R (T B a x b) y (T B c z d)
      (B, a, x, T R b y (T R c z d)) -> T R (T B a x b) y (T B c z d)
      _                              -> T c l val r

------------------------------------------------------------------------------

color t = case t of
  T c _ _ _ -> c
  E         -> B

root t = case t of
  T _ _ x _ -> x
  E         -> Debug.crash "root"

left t = case t of
  T _ l _ _ -> l
  E         -> Debug.crash "left"

right t = case t of
  T _ _ _ r -> r
  E         -> Debug.crash "right"

height t = case t of
  E         -> 0
  T _ l _ r -> 1 + max (height l) (height r)

size t = case t of
  E         -> 0
  T _ l _ r -> 1 + size l + size r

{- BUGGY VERSION: --------------------------------

bso t = case t of
  E         -> True
  T _ l x r -> (l == E || root l < x) &&
               (r == E || x < root r) &&
               bso l && bso r

-------------------------------------------------}

bso t =
  let nonDecreasing xs =
    case xs of
      x1::x2::rest -> x1 <= x2 && nonDecreasing (x2::rest)
      _            -> True
  in
  nonDecreasing (toList t)

toList : Tree a -> List a
toList t = case t of
  E                -> []
  T _ left x right -> toList left ++ [x] ++ toList right


blackHeight t = case t of
  E -> Just 0
  T c l _ r ->
    blackHeight l |> Maybe.andThen (\n ->
    blackHeight r |> Maybe.andThen (\m ->
      if n /= m then Nothing
      else if c == B then Just (1 + n)
      else Just n
    ))

okBlackHeight t = case blackHeight t of
  Just _  -> True
  Nothing -> False

bh t =
  case blackHeight t of
    Just n  -> n
    Nothing -> Debug.crash "bh"

noRedRed t = case t of
  E                   -> True
  T R (T R _ _ _) _ _ -> False
  T R _ _ (T R _ _ _) -> False
  T _ l _ r           -> noRedRed l && noRedRed r

oneRedRed t = case t of
  E                             -> False
  T R (T R _ _ _) _ (T R _ _ _) -> False
  T R (T R l1 _ r1) _ r         -> noRedRed l1 && noRedRed r1 && noRedRed r
  T R l _ (T R l2 _ r2)         -> noRedRed l && noRedRed l2 && noRedRed r2
  T _ l _ r                     -> False

maybeOneRedRed t = oneRedRed t || noRedRed t

rb t = bso t && noRedRed t && okBlackHeight t

rbExceptMaybeRoot t = bso t && maybeOneRedRed t && okBlackHeight t

------------------------------------------------------------------------------
