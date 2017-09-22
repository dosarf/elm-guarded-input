module Guarded.Input exposing (..)

import Html
import Html.Events
import Guarded.Input.InternalTypes exposing (..)


-- Types


type alias Model value =
    Model_ value


type alias Msg value =
    Msg_ value



-- Model initializers


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



-- Html.Attribute for defining onInput event handler


parseOnInput : (Msg value -> msg) -> (String -> Msg value) -> Html.Attribute msg
parseOnInput messageTag parser =
    let
        onInputHandler =
            parser >> messageTag
    in
        Html.Events.onInput onInputHandler



-- update


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



-- utility functions for the model


inputStringMaybe : Model value -> Maybe String
inputStringMaybe (Model_ model) =
    case model.parsedInput of
        Valid_ value ->
            Just <| toString value

        WorkInProgress_ acceptedInputSoFar ->
            Just acceptedInputSoFar

        Undefined_ ->
            Nothing


inputString : Model value -> String
inputString model =
    Maybe.withDefault "" <| inputStringMaybe model


lastError : Model value -> Maybe String
lastError (Model_ model) =
    case model.lastError of
        Just { input, info } ->
            Just info

        Nothing ->
            Nothing


toResult : Model value -> Result String value
toResult (Model_ model) =
    case model.parsedInput of
        Valid_ value ->
            Ok value

        WorkInProgress_ input ->
            "'" ++ input ++ "' is work in progress" |> Err

        Undefined_ ->
            Err "No value is defined"



-- generic input parsing gadgetry
-- TODO comment guard: accepts for feasible (work-in-progress or valid) input (-> Nothing), rejects all else (-> Just error)
-- TODO comment convert: converts all valid text to valid value (or fails)
-- TODO comment: any input that wasn't rejected by the guard but failed to convert is considered WiP input


parser : (String -> Maybe String) -> (String -> Result String value) -> String -> Msg value
parser guard convert input =
    if input == "" then
        UndefinedMsg_
    else
        let
            rejectionResult =
                guard input

            conversionResult =
                convert input
        in
            case ( rejectionResult, conversionResult ) of
                ( Just error, _ ) ->
                    InvalidMsg_ ( input, error )

                ( Nothing, Err error ) ->
                    WorkInProgressMsg_ ( input, error )

                ( Nothing, Ok v ) ->
                    ValidMsg_ v
