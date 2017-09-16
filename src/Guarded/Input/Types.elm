module Guarded.Input.Types exposing (Model(..), Msg(..), inputStringMaybe, inputString, lastError, toResult, fromParsedInput, fromParsedInputAndLastError)

import Guarded.Input.InternalTypes exposing (..)


type Model value
    = Model
        { parsedInput : ParsedInput value
        , lastError : Maybe LastError
        }


type Msg value
    = Valid_ value
    | Invalid_ ( Input, Info )
    | WorkInProgress_ ( Input, Info )
    | Undefined_


inputStringMaybe : Model value -> Maybe String
inputStringMaybe (Model model) =
    case model.parsedInput of
        Valid value ->
            Just <| toString value

        WorkInProgress acceptedInputSoFar ->
            Just acceptedInputSoFar

        Undefined ->
            Nothing


inputString : Model value -> String
inputString model =
    Maybe.withDefault "" <| inputStringMaybe model


lastError : Model value -> Maybe String
lastError (Model model) =
    case model.lastError of
        Just { input, info } ->
            Just info

        Nothing ->
            Nothing


toResult : Model value -> Result String value
toResult (Model model) =
    case model.parsedInput of
        Valid value ->
            Ok value

        WorkInProgress input ->
            "'" ++ input ++ "' is work in progress" |> Err

        Undefined ->
            Err "No value is defined"


fromParsedInput : ParsedInput value -> Model value
fromParsedInput parsedInput =
    Model { parsedInput = parsedInput, lastError = Nothing }


fromParsedInputAndLastError : ParsedInput value -> LastError -> Model value
fromParsedInputAndLastError parsedInput lastError =
    Model { parsedInput = parsedInput, lastError = Just lastError }
