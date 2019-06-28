module Guarded.Input exposing
    ( Model, Msg
    , init, initFor, initWith
    , parseOnInput
    , update
    , inputString, inputStringMaybe, lastError, toResult
    , parser
    )

{-| This library provides support for guarded input (text) controls.
Guarded here means that the input control is simply not allowed to
contain any erroneous string. This is solved by normalization of
the input control's content model on every input event and by feeding back
the normalized model to the view of the input control.

Potentially handy in educational software, where one does not want to confuse kids
with explanations of badly formed input and such like.

A guarded input can be in one of three _acceptable_ states:

  - undefined (empty input control),
  - work-in-progress (not convertible to a useful value, but could evolve
    potentially into one), and
  - valid (has actual value).

If the `Guarded.Input.update` function rejects a change of the input contents,
the previous state - necessarily an _accepted_ one - is preserved, and that
is the model to write back to the view, and this way erroneous
attempts will be just discarded.


# Types

@docs Model, Msg


# Initializers

@docs init, initFor, initWith


# View

@docs parseOnInput


# Model update

@docs update


# Utility functions

@docs inputString, inputStringMaybe, lastError, toResult


# Parsing gadgetry

@docs parser

-}

import Guarded.Input.InternalTypes as InternalTypes exposing (LastError_, Model_, Msg_(..))
import Html
import Html.Events



-- Types


{-| The model for holding data for a guarded input control. Use utility
functions `inputString`, `inputStringMaybe`, `toResult` or `lastError` to
gain access to information held within. To manage CSS classes based on state,
check out package 'Guarded.Input.CssUtil'.
-}
type alias Model value =
    InternalTypes.Model_ value


{-| The message type emitted by a guarded input control.
-}
type alias Msg value =
    InternalTypes.Msg_ value



-- Initializers


{-| Initializes the model with empty (undefined) input.
-}
init : Model value
init =
    InternalTypes.Model_
        { parsedInput = InternalTypes.Undefined_
        , lastError = Nothing
        }


{-| Initializes the model for an actual value and its string
representation.
-}
initFor : value -> String -> Model value
initFor value stringRepresentation =
    InternalTypes.Model_
        { parsedInput = InternalTypes.Valid_ ( value, stringRepresentation )
        , lastError = Nothing
        }


{-| Initializes the model from an input string, using a parser. The model
is in the exact state as if user had typed (copy-pasted) the initial input
string to a guarded input control.

For the sake of consistency of your initialized model, you want to use the very
same parser during model initialization that you use for the input control:

    type MyStuff = ...

    initialModel =
        { ...
        , parsedStuff = Guarded.Input.initWith myStuffParser "stuff"
        }

    ...

    view : Model -> Html Msg
    view model =
        input
            [ Guarded.Input.parseOnInput msgTag myStuffParser
            , ...
            ]
            []
    ...
    myStuffParser : String -> Guarded.Input.Msg MyStuff
    myStuffParser =
        ...

-}
initWith : (String -> Msg value) -> String -> Model value
initWith parserArg initialInput =
    let
        msg =
            parserArg initialInput

        ( firstModel, _ ) =
            update msg init
    in
    firstModel



-- View


{-| Given a message tag with a payload of type `Msg value`, and parser function,
it will return an `Html.Attribute` that you can use with your input control.

    input
        [ Guarded.Input.parseOnInput YourMessageTag someParser
        , value <| Guarded.Input.inputString someParsedModel
        ]
        []

-}
parseOnInput : (Msg value -> msg) -> (String -> Msg value) -> Html.Attribute msg
parseOnInput messageTag parserArg =
    let
        onInputHandler =
            parserArg >> messageTag
    in
    Html.Events.onInput onInputHandler



-- Model update


{-| Updates the model for a guarded input control.
-}
update : Msg v -> Model v -> ( Model v, Cmd (Msg v) )
update message (InternalTypes.Model_ model) =
    case message of
        InternalTypes.ValidMsg_ value ->
            ( InternalTypes.Model_
                { parsedInput = InternalTypes.Valid_ value
                , lastError = Nothing
                }
            , Cmd.none
            )

        InternalTypes.InvalidMsg_ ( input, info ) ->
            ( InternalTypes.Model_
                { model
                    | lastError = Just <| InternalTypes.LastError_ input info
                }
            , Cmd.none
            )

        InternalTypes.WorkInProgressMsg_ ( input, info ) ->
            ( InternalTypes.Model_
                { parsedInput = InternalTypes.WorkInProgress_ input
                , lastError = Nothing
                }
            , Cmd.none
            )

        InternalTypes.UndefinedMsg_ ->
            ( init
            , Cmd.none
            )



-- Utility functions


{-| Returns the input text, as accepted so far (either work-in-progress or valid).
In case of undefined (empty) content of the guarded input control,
it returns `Nothing`.
-}
inputStringMaybe : Model value -> Maybe String
inputStringMaybe (InternalTypes.Model_ model) =
    case model.parsedInput of
        InternalTypes.Valid_ ( value, input ) ->
            Just input

        InternalTypes.WorkInProgress_ acceptedInputSoFar ->
            Just acceptedInputSoFar

        InternalTypes.Undefined_ ->
            Nothing


{-| Returns the input text, as accepted so far (either work-in-progress or valid).
In case of undefined (empty) content of the guarded input control,
it returns an empty string.

You should feed back the model to the view. This way any erroneous input attempt
is rejected, and the input control can contain only acceptable input strings:

    Html.input [ ..., value <| Guarded.Input.inputString myModel ] []

-}
inputString : Model value -> String
inputString model =
    Maybe.withDefault "" <| inputStringMaybe model


{-| Returns the last error info, if any, from a guarded input control model.
-}
lastError : Model value -> Maybe String
lastError (InternalTypes.Model_ model) =
    case model.lastError of
        Just { input, info } ->
            Just info

        Nothing ->
            Nothing


{-| Returns the value, if any, yielded by a guarded input control. In case
of work-in-progress or undefined state, it returns Err.
-}
toResult : Model value -> Result String value
toResult (InternalTypes.Model_ model) =
    case model.parsedInput of
        InternalTypes.Valid_ ( value, input ) ->
            Ok value

        InternalTypes.WorkInProgress_ input ->
            "'" ++ input ++ "' is work in progress" |> Err

        InternalTypes.Undefined_ ->
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
        InternalTypes.UndefinedMsg_

    else
        let
            conversionResult =
                convert input

            wip =
                isWorkInProgress input
        in
        case ( conversionResult, wip ) of
            ( Ok v, _ ) ->
                InternalTypes.ValidMsg_ ( v, input )

            ( Err error, True ) ->
                InternalTypes.WorkInProgressMsg_ ( input, error )

            ( Err error, False ) ->
                InternalTypes.InvalidMsg_ ( input, error )
