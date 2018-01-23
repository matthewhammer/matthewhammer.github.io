module PhysicistsQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)

import Lazy exposing (Lazy, lazy, force)

type Queue a = Q (List a) Int (Lazy (List a)) Int (List a)

empty : Queue a
empty = Q [] 0 (lazy (\_ -> [])) 0 []

isEmpty : Queue a -> Bool
isEmpty (Q _ i _ _ _) = i == 0

enqueue : a -> Queue a -> Queue a
enqueue x (Q pre i front j back) = check pre i front (j+1) (x::back)

dequeue : Queue a -> Maybe (Queue a)
dequeue (Q pre i front j back) =
  if i == 0 then Nothing
  else
    let pre_   = tail pre in
    let front_ = lazy (\_ -> tail (force front)) in
    Just (check pre_ (i-1) front_ j back)

peek : Queue a -> Maybe a
peek (Q pre _ _ _ _) = List.head pre

check pre i front j back =
  if j <= i then checkPre pre i front j back
  else
    let front_ = lazy (\_ -> force front ++ List.reverse back) in
    checkPre pre (i+j) front_ 0 []

checkPre pre i front j back =
  case pre of
    [] -> Q (force front) i front j back
    _  -> Q pre i front j back

tail = fromJust << List.tail

fromJust mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash "fromJust"

-- checkPre pre i front j back =
--   case (pre, i) of
--     ([], 0) -> Q pre i front j back
--     ([], _) -> Q (force front) i (lazy (\_ -> [])) j back
--     _       -> Q pre i front j back

