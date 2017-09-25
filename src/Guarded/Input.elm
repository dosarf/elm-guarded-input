module Guarded.Input exposing (..)

{-| This library provides support for guarded input (text) controls.
A guarded input can be in one of three states:
- undefined (empty input control),
- work-in-progress (not convertible to a useful value, but could evolve
potentially into one), and
- valid (has actual value).

Most notably it cannot be in an invalid state - any such erroneous change is
rejected by the model, the previous, necessarily valid state is preserved.

To reflect the model (which may reject the latest attempted input, preserving
the last acceptable state) in the view, the view function needs to make sure
to use the model for the value attribute for `Html.input`, see `inputString`.

Potentially handy in educational software, where one does not want to confuse kids
with explanations of badly formed input and such like. Of course, one can
still get info on last erroneous input attempt, if needed, see `lastError`.


# Types
@docs Model, Msg

# Initializers
@docs init, initFor

# view
@docs parseOnInput

# Model update
@docs update

# Utility functions
@docs inputString, inputStringMaybe, lastError, toResult

# Parsing gadgetry
@docs parser
-}

import Html
import Html.Events
import Guarded.Input.InternalTypes exposing (..)


-- Types


{-| The model for holding data for a guarded input control. It holds parsed
(verified and converted) value (if any), or the partial (work-in-progress)
input text (if any). It also holds information about the error of the last input
attempt (if any). Use utility functions `inputStringMaybe`,
`inputString`, `toResult` or `lastError` to gain access to information held within.
-}
type alias Model value =
    Model_ value


{-| The message type emitted by a guarded input control.
-}
type alias Msg value =
    Msg_ value



-- Initializers


{-| Initializes the model with empty (undefined) input and no recorded error
for last input attempt.
-}
init : Model value
init =
    Model_
        { parsedInput = Undefined_
        , lastError = Nothing
        }


{-| Initializes the model for an actual value. There is no recorded error for
last input attempt.
-}
initFor : value -> Model value
initFor value =
    Model_
        { parsedInput = Valid_ value
        , lastError = Nothing
        }



-- View


{-| Given a message tag with a payload of type `Msg value`, and parser function,
it will return an `Html.Attribute`.

        input
            [ Guarded.Input.parseOnInput YourMessageTag someParser
            , value <| Guarded.Input.inputString someParsedValue
            ]
            []
-}
parseOnInput : (Msg value -> msg) -> (String -> Msg value) -> Html.Attribute msg
parseOnInput messageTag parser =
    let
        onInputHandler =
            parser >> messageTag
    in
        Html.Events.onInput onInputHandler



-- Model update


{-| Updates the model for a guarded input control.
- In case the Msg delivers an event about an erroneous input attempted, the
  value and/or the currently accepted input text will be left unupdated, but
  the info about the last error is preserved in the model.
-}
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



-- Utility functions


{-| Returns the input text, as accepted so far  of a guarded input control.
In case of undefined (empty) content of the guarded input control,
it returns `Nothing`.
-}
inputStringMaybe : Model value -> Maybe String
inputStringMaybe (Model_ model) =
    case model.parsedInput of
        Valid_ value ->
            Just <| toString value

        WorkInProgress_ acceptedInputSoFar ->
            Just acceptedInputSoFar

        Undefined_ ->
            Nothing


{-| Returns the input text, as accepted so far (the input text could be
work-in-progress (=not yet convertible to a value), or an actual valid string
(= convertible to a value)) of a guarded input control.
In case of undefined (empty), input control, returns an empty string.

Most useful to feed the currently accepted input text from model (which may have
rejected a previous erroneous input attempt) back to the value of the guarded
input control:

    Html.input [ ..., value <| Guarded.Input.inputString myModel ] []

-}
inputString : Model value -> String
inputString model =
    Maybe.withDefault "" <| inputStringMaybe model


{-| Returns the last error info, if any, from a guarded input control model.
-}
lastError : Model value -> Maybe String
lastError (Model_ model) =
    case model.lastError of
        Just { input, info } ->
            Just info

        Nothing ->
            Nothing


{-| Returns the value, if any, yielded by a guarded input control. In case
of work-in-progress or undefined state, it return Err.
-}
toResult : Model value -> Result String value
toResult (Model_ model) =
    case model.parsedInput of
        Valid_ value ->
            Ok value

        WorkInProgress_ input ->
            "'" ++ input ++ "' is work in progress" |> Err

        Undefined_ ->
            Err "Undefined"



-- Parsing gadgetry


{-| A function to construct your own parsers for guarded input controls. Needs
a `convert` and an `isWorkInProgress` function.

The `convert` function (`String -> Result String value`) attemps converting any
non-empty input text. On failure, it is to return an error message.

The `isWorkInProgress` function (`String -> Boolean`) is to accept any non-empty
input text rejected by the `convert` that could still evolve into something
valid. For instance, while 'tru' cannot be converted to a Bool value (hence would
be rejected by `convert`), it may still end up being 'true', thus the
`isWorkInProgress` should accept it as work-in-progress input.

A non-empty input string rejected by both `convert` and `isWorkInProgress` is invalid.

A non-empty input string rejected `convert` but accepted by `isWorkInProgress` is
accepted as work in progress.

A non-empty input string converted successfully to a value by `convert` is
valid. In this case it does not matter whether `isWorkInProgress` accepts it or
rejects it
-}
parser : (String -> Result String value) -> (String -> Bool) -> String -> Msg value
parser convert isWorkInProgress input =
    if input == "" then
        UndefinedMsg_
    else
        let
            conversionResult =
                convert input

            wip =
                isWorkInProgress input
        in
            case ( conversionResult, wip ) of
                ( Ok v, _ ) ->
                    ValidMsg_ v

                ( Err error, True ) ->
                    WorkInProgressMsg_ ( input, error )

                ( Err error, False ) ->
                    InvalidMsg_ ( input, error )
