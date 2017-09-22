module Guarded.Input.Types exposing (Model, Msg, inputStringMaybe, inputString, lastError, toResult, fromParsedInput, fromParsedInputAndLastError)

import Guarded.Input.InternalTypes exposing (..)


type alias Model value =
    Model_ value


type alias Msg value =
    Msg_ value


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


fromParsedInput : ParsedInput_ value -> Model value
fromParsedInput parsedInput =
    Model_ { parsedInput = parsedInput, lastError = Nothing }


fromParsedInputAndLastError : ParsedInput_ value -> LastError_ -> Model value
fromParsedInputAndLastError parsedInput lastError =
    Model_ { parsedInput = parsedInput, lastError = Just lastError }
