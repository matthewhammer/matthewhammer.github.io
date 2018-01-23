module IntroML exposing (..)

exclaim s = s ++ "!"

plus x y = x + y

plusInt : Int -> Int -> Int
plusInt = plus

choose : Bool -> thing -> thing -> thing
choose b x y = if b then x else y

chooseNum : Bool -> number -> number -> number
chooseNum = choose

id5 : (t1,t2,t3,t4,t5) -> (t1,t2,t3,t4,t5)
id5 (a,b,c,d,e) = (a,b,c,d,e)

-- plus3 a =
--   let b = a + 1 in
--   let c = b + 1 in
--   let d = c + 1 in
--     d

-- plus3 a =
--   let b = a + 1
--       c = b + 1
--       d = c + 1
--   in
--     d

-- plus3 a = a |> plus 1 |> plus 1 |> plus 1

plus3 = plus 1 << plus 1 << plus 1

firstTwo xs =
  case xs of
    x::y::_ -> (x, y)
    _       -> Debug.crash "firstTwo"

maybeHead : List a -> Maybe a
maybeHead xs = case xs of
  x::xs_ -> Just x
  []     -> Nothing

maybeTail : List a -> Maybe (List a)
maybeTail xs = case xs of
  x::xs_ -> Just xs_
  []     -> Nothing

errHead : List a -> Result String a
errHead xs = case xs of
  x::_ -> Ok x
  []   -> Err "errHead: expects non-empty list"

errTail : List a -> Result String (List a)
errTail xs = case xs of
  _::xs_ -> Ok xs_
  []     -> Err "errTail: expects non-empty list"

