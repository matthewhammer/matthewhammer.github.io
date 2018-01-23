module Contracts exposing (..)

------------------------------------------------------------------------------

violation func name values =
  Debug.crash <|
    String.join "\n"
      [ ""
      , func
      , "assertion violation for: " ++ name
      , toString values
      ]

monitor : String -> (a -> Bool) -> (a -> b -> Bool) -> (a -> b) -> (a -> b)
monitor f pre post foo =
  \x ->
    if pre x == False then violation f "1st arg" x
    else
      let ret = foo x in
      if post x ret == False
      then violation f "return value" (x, ret)
      else ret

maybeMonitor flag f pre post foo =
  if flag then monitor f pre post foo else foo

------------------------------------------------------------------------------

makeMaybeMonitors flag =
  { one = maybeMonitor flag
  , two = maybeMonitor2 flag
  , three = maybeMonitor3 flag
  , four = maybeMonitor4 flag
  }

------------------------------------------------------------------------------

monitor2 : String -> (a -> Bool) -> (a -> b -> Bool) -> (a -> b -> c -> Bool) -> (a -> b -> c) -> (a -> b -> c)
monitor2 f pre1 pre2 post foo =
  \x1 x2 ->
    if pre1 x1 == False then violation f "1st arg" x1
    else if pre2 x1 x2 == False then violation f "2nd arg" (x1, x2)
    else
      let ret = foo x1 x2 in
      if post x1 x2 ret == False
      then violation f "return value" (x1, x2, ret)
      else ret

monitor3 f pre1 pre2 pre3 post foo =
  \x1 x2 x3 ->
    if pre1 x1 == False then violation f "1st arg" x1
    else if pre2 x1 x2 == False then violation f "2nd arg" (x1, x2)
    else if pre3 x1 x2 x3 == False then violation f "3rd arg" (x1, x2, x3)
    else
      let ret = foo x1 x2 x3 in
      if post x1 x2 x3 ret == False
      then violation f "return value" (x1, x2, x3, ret)
      else ret

monitor4 f pre1 pre2 pre3 pre4 post foo =
  \x1 x2 x3 x4 ->
    if pre1 x1 == False then violation f "1st arg" x1
    else if pre2 x1 x2 == False then violation f "2nd arg" (x1, x2)
    else if pre3 x1 x2 x3 == False then violation f "3rd arg" (x1, x2, x3)
    else if pre4 x1 x2 x3 x4 == False then violation f "4th arg" (x1, x2, x3, x4)
    else
      let ret = foo x1 x2 x3 x4 in
      if post x1 x2 x3 x4 ret == False
      then violation f "return value" (x1, x2, x3, x4, ret)
      else ret

maybeMonitor2 flag f pre1 pre2 post foo =
  if flag then monitor2 f pre1 pre2 post foo else foo

maybeMonitor3 flag f pre1 pre2 pre3 post foo =
  if flag then monitor3 f pre1 pre2 pre3 post foo else foo

maybeMonitor4 flag f pre1 pre2 pre3 pre4 post foo =
  if flag then monitor4 f pre1 pre2 pre3 pre4 post foo else foo

