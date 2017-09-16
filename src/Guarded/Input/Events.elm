module Guarded.Input.Events exposing (..)

import Html
import Html.Events
import Guarded.Input.Types exposing (..)


parseOnInput : (Msg value -> msg) -> (String -> Msg value) -> Html.Attribute msg
parseOnInput messageTag parser =
    let
        onInputHandler =
            parser >> messageTag
    in
        Html.Events.onInput onInputHandler
