module MoreTrees exposing (..)

type Tree a = Empty | Node a (Tree a) (Tree a)

height t =
  case t of
    Empty        -> 0
    Node _ t1 t2 -> 1 + max (height t1) (height t2)

------------------------------------------------------------------------------

isFull t =
  let h = height t in
  let foo depth tree =
    if depth == h then True
    else
      case tree of
        Empty -> False
        Node _ t1 t2 ->
          let b1 = foo (depth+1) t1 in
          let b2 = foo (depth+1) t2 in
          b1 && b2
  in
  h == 0 || foo 0 t

------------------------------------------------------------------------------

isFullTree : Tree a -> Bool
isFullTree t =
  case maybeFullTreeHeight t of
    Just _  -> True
    Nothing -> False
    
maybeFullTreeHeight : Tree a -> Maybe Int
maybeFullTreeHeight t =
  case t of
    Empty -> Just 0
    Node _ left right ->
      maybeFullTreeHeight left  |> maybeAndThen (\h1 ->
      maybeFullTreeHeight right |> maybeAndThen (\h2 ->
      maybeGuard (h1 == h2)     |> maybeAndThen (\() ->
        maybeReturn (1 + h1)
      )))

{-
      case maybeFullTreeHeight left of
        Nothing -> Nothing
        Just h1 ->
          case maybeFullTreeHeight right of
            Nothing -> Nothing
            Just h2 ->
              if h1 == h2
                then Just (1 + h1)
                else Nothing
-}


------------------------------------------------------------------------------

maybeAndThen : (a -> Maybe b) -> Maybe a -> Maybe b
maybeAndThen f mx =
  case mx of
    Nothing -> Nothing
    Just x  -> f x

maybeReturn : a -> Maybe a
maybeReturn x = Just x

maybeGuard : Bool -> Maybe ()
maybeGuard b =
  if b
    then Just ()
    else Nothing


------------------------------------------------------------------------------

maybeMaybeAndThen : (a -> Maybe (Maybe b)) -> Maybe (Maybe a) -> Maybe (Maybe b)
maybeMaybeAndThen f mmx =
  case mmx of
    Nothing -> Nothing
    Just mx -> mx |> maybeAndThen f

maybeMaybeGuard : Bool -> Maybe (Maybe ())
maybeMaybeGuard b =
  if b
    then Just (Just ())
    else Nothing

maybeMaybeReturn : a -> Maybe (Maybe a)
maybeMaybeReturn x = Just (Just x)









------------------------------------------------------------------------------
-- NOTE: clunky solution

isFullOnePass : Tree a -> Bool
isFullOnePass t =
  case traverse (Just Nothing) 0 t of
    Just (Just _) -> True
    _             -> False

traverse : Maybe (Maybe Int) -> Int -> Tree a -> Maybe (Maybe Int)
traverse acc depth tree =
  case (acc, tree) of
    (Nothing, _) -> Nothing
    (Just Nothing, Empty) -> Just (Just depth)
    (Just (Just maxDepth), Empty) ->
      if depth == maxDepth then acc else Nothing
    (acc, Node _ t1 t2) ->
      let recurse = traverse acc (depth+1) in
      recurse t1               |> maybeMaybeAndThen (\n ->
      recurse t2               |> maybeMaybeAndThen (\m ->
      maybeMaybeGuard (n == m) |> maybeMaybeAndThen (\_ ->
        maybeMaybeReturn n
      )))
{-
      case recurse t1 of
        Nothing -> Nothing
        Just Nothing -> Nothing
        Just (Just n) ->
          case recurse t2 of
            Nothing -> Nothing
            Just Nothing -> Nothing
            Just (Just m) -> if n == m then Just (Just n) else Nothing
-}
