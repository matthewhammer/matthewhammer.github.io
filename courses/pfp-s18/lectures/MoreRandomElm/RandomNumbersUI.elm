module RandomNumbersUI exposing (makeProgram)

import Html exposing (Html)
import Html.Attributes as Attr
import Mouse
import Keyboard


type alias Model_ a = { a | randomNumbers : List Int }


makeProgram : (Model_ a, Cmd msg)
           -> (msg -> Model_ a -> (Model_ a, Cmd msg))
           -> (msg, msg, msg)
           -> List (Sub msg)
           -> Program Never (Model_ a) msg
makeProgram init update commonMessages moreSubscriptions =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = makeSubscriptions commonMessages moreSubscriptions
    }


makeSubscriptions : (msg, msg, msg) -> List (Sub msg) -> model -> Sub msg
makeSubscriptions (mouseClick, reset, noop) moreSubscriptions _ =
  Sub.batch <|
    [ Mouse.clicks (always mouseClick)
    , Keyboard.downs (\keyCode -> if keyCode == 27 then reset else noop)
    ] ++ moreSubscriptions


view : Model_ a -> Html msg
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
  let display =
    Html.text ("Random Numbers: " ++ toString (List.reverse model.randomNumbers))
  in
  Html.div [style] [display]
