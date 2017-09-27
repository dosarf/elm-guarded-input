module Guarded.Input exposing (..)

{-| This library provides support for guarded input (text) controls.
Guarded here means that the input (text) control is simply not allowed to
contain any erroneous string. This is solved by constant normalization of
the input control's content model and by feeding back the normalized model
to the view of the input control.

Potentially handy in educational software, where one does not want to confuse kids
with explanations of badly formed input and such like.

A guarded input can be in one of three _acceptable_ states:
- undefined (empty input control),
- work-in-progress (not convertible to a useful value, but could evolve
potentially into one), and
- valid (has actual value).

If the `Guarded.Input.update` function rejects a change of the input contents,
the previously accepted model is preserved, and that model should be written back
to the view - this way erroneous attempts will be just discarded.

## What's with work-in-progress?
Erroneous content of input control is disallowed (actually, discarded immediately,
as explained above). But there are contents that, while themselves are not yet
convertible to a valid value, can evolve into such. For instance, a single
minus character ("-") can evolve into a valid negative number. Such content
must be allowed in the input control. The model in this case does not have
a valid value to use (how could it?), but the work-in-progress string is
stored nevertheless in the model, to be able to feed it back to the input
control when generating the view. See `Guarded.Input.inputString`.

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


{-| The model for holding data for a guarded input control. Use utility
functions `inputStringMaybe`, `inputString`, `toResult` or `lastError` to
gain access to information held within.
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
        { parsedInput = Valid_ ( value, toString value )
        , lastError = Nothing
        }



-- View


{-| Given a message tag with a payload of type `Msg value`, and parser function,
it will return an `Html.Attribute` - an `Html.Events.onInput` with the
event hanlder constructed out of the parser and the function converting
`Guarded.Input.Msg` into your message type (effectively a message tag of yours).

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

In case the Msg delivers an event about an erroneous input attempted, the
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
        Valid_ ( value, input ) ->
            Just input

        WorkInProgress_ acceptedInputSoFar ->
            Just acceptedInputSoFar

        Undefined_ ->
            Nothing


{-| Returns the input text, as accepted so far (the input text could be
work-in-progress (=not yet convertible to a value), or an actual valid string
(= convertible to a value)) of a guarded input control.
In case of undefined (empty), input control, returns an empty string.

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
        Valid_ ( value, input ) ->
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
                    ValidMsg_ ( v, input )

                ( Err error, True ) ->
                    WorkInProgressMsg_ ( input, error )

                ( Err error, False ) ->
                    InvalidMsg_ ( input, error )
