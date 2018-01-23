module TestRedBlackTree exposing (..)

import String
import Expect
import Test exposing (..)
import Test.Runner.Html
import Fuzz exposing (..)
import Char

-- import RedBlackTree exposing (..)
import RedBlackTreeContracts exposing (..)


toList : Tree a -> List a
toList t = case t of
  E                -> []
  T _ left x right -> toList left ++ [x] ++ toList right


fromList : List comparable -> Tree comparable
fromList xs = List.foldl insert empty xs


sortAndRemoveDupes : List comparable -> List comparable
sortAndRemoveDupes =
  let removeDupes xs =
    case xs of
      []         -> []
      [x]        -> [x]
      x::y::rest -> if x == y
                    then removeDupes (y::rest)
                    else x :: removeDupes (y::rest)
  in
  List.sort >> removeDupes


sortAndRemoveDupes : List comparable -> Expectation
insertThenCheckTree xs =
  let tree = fromList xs in
  let checkSortedElements =
    let result = toList tree in
    let expected = sortAndRemoveDupes xs in
    let errorMessage = 
      String.join "\n\n" <|
        [ "Checking that all elements are found and in sorted order"
        , "toList <| " ++ toString tree
        , " == " ++ toString result
        , " /= " ++ toString expected
        ]
    in
    result
      |> Expect.equal expected
      |> Expect.onFail errorMessage
  in
  let checkHeight =
    case blackHeight tree of
      Nothing -> Expect.pass
      Just bh ->
        let h = height tree in
        let errorMessage = 
          String.join "\n\n" <|
            [ "Checking that h <= 2*bh + 1"
            , toString tree
            , " h = " ++ toString h
            , " bh = " ++ toString bh
            ]
        in
        h |> Expect.atMost (2 * bh + 1)
          |> Expect.onFail errorMessage
  in
  let checkNoRedRed =
    let errorMessage =
      String.join "\n\n" <|
        [ "Checking no red-red violations", toString tree ]
    in
    noRedRed tree
      |> Expect.equal True
      |> Expect.onFail errorMessage
  in
  Expect.all
    (List.map always [ checkSortedElements
                     , checkHeight
                     , checkNoRedRed
                     ]) ()


main : Test.Runner.Html.TestProgram
main =
  [ fuzzWith { runs = 1000 } (list int)
      "1,000 randomly generated trees"
      insertThenCheckTree
  , fuzzWith { runs = 1000 } (list (intRange -10000 10000))
      "1,000 more randomly generated trees"
      insertThenCheckTree
  ]
  |> concat
  |> Test.Runner.Html.run
