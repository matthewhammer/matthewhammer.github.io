module MoreRandom exposing (main)

import Random
import RandomNumbersUI


main =
  RandomNumbersUI.makeProgram
    (initialModel, Cmd.none) update (MouseClick, Reset, Noop) []


type Msg = Noop | Reset | MouseClick | RandomNumber Int

type alias Model = { randomNumbers: List Int }

initialModel = { randomNumbers = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initialModel, Cmd.none)
    MouseClick -> (model, Random.generate RandomNumber (Random.int 1 10))
    RandomNumber i -> ({ randomNumbers = i :: model.randomNumbers }, Cmd.none)

