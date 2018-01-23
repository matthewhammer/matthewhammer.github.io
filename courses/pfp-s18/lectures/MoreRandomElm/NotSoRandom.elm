module NotSoRandom exposing (main)

import Random exposing (Seed)
import RandomNumbersUI


main =
  RandomNumbersUI.makeProgram
    (initialModel, Cmd.none) update (MouseClick, Reset, Noop) []


type Msg = Noop | Reset | MouseClick

type alias Model = { seed: Seed, randomNumbers: List Int }

initialModel = { seed = Random.initialSeed 17, randomNumbers = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initialModel, Cmd.none)
    MouseClick ->
      let (i, newSeed) = Random.step (Random.int 1 10) model.seed in
      let randomNumbers = i :: model.randomNumbers in
      ({ seed = newSeed, randomNumbers = randomNumbers }, Cmd.none)
