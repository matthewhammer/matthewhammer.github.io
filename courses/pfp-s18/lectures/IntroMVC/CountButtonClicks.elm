-- commented for online playground; uncomment to compile
--
-- module CountButtonClicks exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = { count: Int }

initialModel = { count = 0 }


-- UPDATE

type Msg = Reset | Increment

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> initialModel
    Increment -> { count = 1 + model.count }


-- VIEW

view : Model -> Html Msg
view model =
  let reset = Html.button [onClick Reset] [Html.text "Reset"] in
  let increment = Html.button [onClick Increment] [Html.text "Increment"] in
  let display = Html.text ("Count: " ++ toString model.count) in
  Html.div [] [reset, increment, display]


-- MAIN

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }
