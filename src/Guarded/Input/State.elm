module Guarded.Input.State exposing (..)

import Guarded.Input.Types exposing (..)
import Guarded.Input.InternalTypes exposing (..)


init : Model value
init =
    Model
        { parsedInput = Undefined
        , lastError = Nothing
        }


initFor : value -> Model value
initFor value =
    Model
        { parsedInput = Valid value
        , lastError = Nothing
        }


update : Msg v -> Model v -> ( Model v, Cmd (Msg v) )
update message (Model model) =
    case message of
        Valid_ value ->
            ( Model
                { parsedInput = Valid value
                , lastError = Nothing
                }
            , Cmd.none
            )

        Invalid_ ( input, info ) ->
            ( Model
                { model
                    | lastError = Just <| LastError input info
                }
            , Cmd.none
            )

        WorkInProgress_ ( input, info ) ->
            ( Model
                { parsedInput = WorkInProgress input
                , lastError = Nothing
                }
            , Cmd.none
            )

        Undefined_ ->
            ( init
            , Cmd.none
            )
