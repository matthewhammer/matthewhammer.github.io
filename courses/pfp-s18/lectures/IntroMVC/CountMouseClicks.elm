module CountMouseClicks exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Mouse
import Keyboard


-- MODEL

type alias Model = { count: Int }

initialModel = { count = 0 }


-- UPDATE

type Msg = Noop | Reset | Increment

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initialModel, Cmd.none)
    Increment -> ({ count = 1 + model.count }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  let style =
    -- https://css-tricks.com/quick-css-trick-how-to-center-an-object-exactly-in-the-center/
    Attr.style <|
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
  in
  let display = Html.text ("Count: " ++ toString model.count) in
  Html.div [style] [display]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks (always Increment)
    , Keyboard.downs (\keyCode -> if keyCode == 27 then Reset else Noop)
    ]


-- MAIN

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
