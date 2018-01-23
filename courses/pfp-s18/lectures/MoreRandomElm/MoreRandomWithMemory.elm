port module MoreRandomWithMemory exposing (main)

import Random
import RandomNumbersUI


main =
  RandomNumbersUI.makeProgram
    (initialModel, requestNumbers ()) update
    (MouseClick, Reset, Noop) [ receiveNumbers LoadNumbers ]

type Msg = Noop | Reset | MouseClick | RandomNumber Int | LoadNumbers (List Int)

type alias Model = { randomNumbers: List Int }

initialModel = { randomNumbers = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initialModel, clearNumbers ())
    LoadNumbers nums -> ({ randomNumbers = nums }, Cmd.none)
    MouseClick -> (model, Random.generate RandomNumber (Random.int 1 10))
    RandomNumber n ->
      let nums = n :: model.randomNumbers in
      ({ randomNumbers = nums }, saveNumbers nums)


-- Ports for loading/clearing/saving numbers in local storage --------

port requestNumbers : () -> Cmd msg
port receiveNumbers : (List Int -> msg) -> Sub msg

port clearNumbers : () -> Cmd msg

port saveNumbers : List Int -> Cmd msg
