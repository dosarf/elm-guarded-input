module Guarded.Input.State exposing (..)

import Guarded.Input.Types exposing (..)
import Guarded.Input.InternalTypes exposing (..)


init : Model value
init =
    Model_
        { parsedInput = Undefined_
        , lastError = Nothing
        }


initFor : value -> Model value
initFor value =
    Model_
        { parsedInput = Valid_ value
        , lastError = Nothing
        }


update : Msg v -> Model v -> ( Model v, Cmd (Msg v) )
update message (Model_ model) =
    case message of
        ValidMsg_ value ->
            ( Model_
                { parsedInput = Valid_ value
                , lastError = Nothing
                }
            , Cmd.none
            )

        InvalidMsg_ ( input, info ) ->
            ( Model_
                { model
                    | lastError = Just <| LastError_ input info
                }
            , Cmd.none
            )

        WorkInProgressMsg_ ( input, info ) ->
            ( Model_
                { parsedInput = WorkInProgress_ input
                , lastError = Nothing
                }
            , Cmd.none
            )

        UndefinedMsg_ ->
            ( init
            , Cmd.none
            )
