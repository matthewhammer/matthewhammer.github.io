module RedBlackDelete exposing (Tree, empty, member, insert, remove)

type Color
  = BB   -- +2  "double black" ("double black")
  | B    -- +1
  | R    --  0
  | RR   -- -1  "double red"   ("negative black")

type Tree a
  = E    --  0  "empty"        ("black leaf"        +1)
  | BE   -- +1  "black empty"  ("double black leaf" +2)
  | T Color (Tree a) a (Tree a)

incr c =
  case c of
    BB -> Debug.crash "incr BB"
    B  -> BB
    R  -> B
    RR -> R

decr c =
  case c of
    BB -> B
    B  -> R
    R  -> RR
    RR -> Debug.crash "decr RR"

empty = E

member : comparable -> Tree comparable -> Bool
member x t = case t of
  E -> False
  T _ l y r ->
    if x == y then True
    else if x < y then member x l
    else member x r
  BE -> Debug.crash "member BE"

insert : comparable -> Tree comparable -> Tree comparable
insert x t =
  case ins x t of
    T _ l y r -> T B l y r
    _         -> Debug.crash "insert E"

ins : comparable -> Tree comparable -> Tree comparable
ins x t =
  case t of
    E -> T R E x E
    T c l y r ->
      if x == y then t
      else if x < y then balance c (ins x l) y r
      else balance c l y (ins x r)
    BE -> Debug.crash "ins BE"

type alias Balance comparable =
  Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable

type alias BalanceMaybe comparable =
  Color -> Tree comparable -> comparable -> Tree comparable -> Maybe (Tree comparable)

balance : Balance comparable
balance c l val r =
  balance_B_R_R c l val r
  |> maybePlus (balance_BB_R_R c l val r)
  |> maybePlus (balance_BB_RR c l val r)
  |> Maybe.withDefault (T c l val r)

balance_B_R_R : BalanceMaybe comparable
balance_B_R_R color l val r =
  case (color, l, val, r) of
    (B, T R (T R a x b) y c, z, d) -> Just <| T R (T B a x b) y (T B c z d)
    (B, T R a x (T R b y c), z, d) -> Just <| T R (T B a x b) y (T B c z d)
    (B, a, x, T R (T R b y c) z d) -> Just <| T R (T B a x b) y (T B c z d)
    (B, a, x, T R b y (T R c z d)) -> Just <| T R (T B a x b) y (T B c z d)
    _                              -> Nothing

balance_BB_R_R : BalanceMaybe comparable
balance_BB_R_R color l val r =
  case (color, l, val, r) of
    (BB, T R (T R a x b) y c, z, d) -> Just <| T B (T B a x b) y (T B c z d)
    (BB, T R a x (T R b y c), z, d) -> Just <| T B (T B a x b) y (T B c z d)
    (BB, a, x, T R (T R b y c) z d) -> Just <| T B (T B a x b) y (T B c z d)
    (BB, a, x, T R b y (T R c z d)) -> Just <| T B (T B a x b) y (T B c z d)
    _                               -> Nothing

balance_BB_RR : BalanceMaybe comparable
balance_BB_RR color l val r =
  case (color, l, val, r) of
    (BB, T RR (T B a w b) x (T B c y d), z, e) -> Just <| T B (balance B (T R a w b) x c) y (T B d z e)
    (BB, a, w, T RR (T B b x c) y (T B d z e)) -> Just <| T B (T B a w b) x (balance B c y (T R d z e))
    _                                          -> Nothing

bubble_BE_and_BB : Tree comparable -> Tree comparable
bubble_BE_and_BB t =
  case t of

    -- cases 1a, 2a, 3a
    T c1 (T c2 a x b) y BE ->
      case (c1, c2) of
        (R, B) -> balance (incr c1) (T (decr c2) a x b) y E
        (B, B) -> balance (incr c1) (T (decr c2) a x b) y E
        (B, R) -> balance (incr c1) (T (decr c2) a x b) y E
        _      -> t

    -- cases 1b, 2b, 3b
    T c1 BE y (T c3 c z d) ->
      case (c1, c3) of
        (R, B) -> balance (incr c1) E y (T (decr c3) c z d)
        (B, B) -> balance (incr c1) E y (T (decr c3) c z d)
        (B, R) -> balance (incr c1) E y (T (decr c3) c z d)
        _      -> t

    -- cases 1a', 1b', 2a', 2b', 3a', 3b'
    T c1 (T c2 a x b) y (T c3 c z d) ->
      case (c1, c2, c3) of
        (R, B, BB) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        (R, BB, B) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        (B, B, BB) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        (B, BB, B) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        (B, R, BB) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        (B, BB, R) -> balance (incr c1) (T (decr c2) a x b) y (T (decr c3) c z d)
        _          -> t

    _ -> t

remove : comparable -> Tree comparable -> Tree comparable
remove x t =
  case rem x t of
    T _ l y r -> T B l y r
    _         -> Debug.crash "remove E"

rem : comparable -> Tree comparable -> Tree comparable
rem n t =
  case t of

    BE -> Debug.crash "rem BE"
    E  -> E

    -- 0 children
    T R E x E -> if n == x then BE else t
    T B E x E -> if n == x then T BB E x E else t

    -- 1 child
    T B (T R E x E) y E -> if n == y then T B E x E else t
    T B E y (T R E z E) -> if n == y then T B E z E else t
    T _ E _ _           -> Debug.crash "rem"
    T _ _ _  E          -> Debug.crash "rem"

    -- 2 children
    T c l y r ->
      if n < y then balance c (bubble_BE_and_BB (rem n l)) y r
      else if n > y then balance c l y (bubble_BE_and_BB (rem n r))
      else {- n == y -} rem_2_children c l y r

rem_2_children : Color -> Tree comparable -> comparable -> Tree comparable -> Tree comparable
rem_2_children c left y right =
  let remove_max t =
    case t of
      T R E x E           -> (x, E)          -- cases 1a/1b
      T B E x E           -> (x, BE)         -- cases 2a/2b
      T B (T R E w E) x E -> (x, T B E w E)  -- cases 3a/3b

      T c l v r ->
        let (x, r2) = remove_max r in
        (x, T c l v r2)

      _ -> Debug.crash "rem_2_children"
  in
  let (x, new_left) = remove_max left in
  T c new_left x right

maybePlus : Maybe a -> Maybe a -> Maybe a
maybePlus mx my =
  case mx of
    Just x  -> mx
    Nothing -> my

