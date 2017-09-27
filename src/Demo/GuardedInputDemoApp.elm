module Demo.GuardedInputDemoApp exposing (..)

import Guarded.Input
import Guarded.Input.Parsers
import Guarded.Input.CssUtil
import Html exposing (Attribute, Html, div, input, text, table, caption, span, thead, tbody, tr, td)
import Html.Attributes exposing (class, classList, value, style)
import Html.Lazy exposing (lazy3)


type alias Model =
    { parsedInt : Guarded.Input.Model Int
    , parsedNonNegativeInt : Guarded.Input.Model Int
    , parsedDigit : Guarded.Input.Model Int
    , parsedFloat : Guarded.Input.Model Float
    , parsedNonNegativeFloat : Guarded.Input.Model Float
    , parsedBool : Guarded.Input.Model Bool
    }


initialModel : Model
initialModel =
    { parsedInt = Guarded.Input.initFor -42
    , parsedNonNegativeInt = Guarded.Input.initFor 42
    , parsedDigit = Guarded.Input.init
    , parsedFloat = Guarded.Input.init
    , parsedNonNegativeFloat = Guarded.Input.initFor 3.1415
    , parsedBool = Guarded.Input.initWith boolParser "ye"
    }


type Msg
    = AnyIntChanged (Guarded.Input.Msg Int)
    | NonNegativeIntChanged (Guarded.Input.Msg Int)
    | DigitChanged (Guarded.Input.Msg Int)
    | FloatChanged (Guarded.Input.Msg Float)
    | NonNegativeFloatChanged (Guarded.Input.Msg Float)
    | BoolChanged (Guarded.Input.Msg Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AnyIntChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedInt
            in
                ( { model
                    | parsedInt = newParsed
                  }
                , Cmd.map AnyIntChanged subCmd
                )

        NonNegativeIntChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedNonNegativeInt
            in
                ( { model
                    | parsedNonNegativeInt = newParsed
                  }
                , Cmd.map NonNegativeIntChanged subCmd
                )

        DigitChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedDigit
            in
                ( { model
                    | parsedDigit = newParsed
                  }
                , Cmd.map DigitChanged subCmd
                )

        FloatChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedFloat
            in
                ( { model
                    | parsedFloat = newParsed
                  }
                , Cmd.map FloatChanged subCmd
                )

        NonNegativeFloatChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedNonNegativeFloat
            in
                ( { model
                    | parsedNonNegativeFloat = newParsed
                  }
                , Cmd.map NonNegativeFloatChanged subCmd
                )

        BoolChanged msg ->
            let
                ( newParsed, subCmd ) =
                    Guarded.Input.update msg model.parsedBool
            in
                ( { model
                    | parsedBool = newParsed
                  }
                , Cmd.map BoolChanged subCmd
                )


view : Model -> Html Msg
view model =
    div []
        [ demoTable model ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


demoTable : Model -> Html Msg
demoTable model =
    table
        [ class "bordered" ]
        [ caption
            []
            [ text "Table with inputs and their parsed state" ]
        , demoTableHead model
        , demoTableBody model
        ]


demoTableHead : Model -> Html Msg
demoTableHead model =
    thead
        []
        ([ "Use case"
         , "Input control"
         , "Accepted input"
         , "Info on last attempted erroneous input"
         ]
            |> List.map (\x -> td [] [ text x ])
        )


demoTableBody : Model -> Html Msg
demoTableBody model =
    tbody
        []
        [ demoTableInputRow "any integer" Guarded.Input.Parsers.intParser AnyIntChanged model.parsedInt
        , demoTableInputRow "non-negative integer" Guarded.Input.Parsers.nonNegativeIntParser NonNegativeIntChanged model.parsedNonNegativeInt
        , demoTableInputRow "single digit" Guarded.Input.Parsers.decimalDigitParser DigitChanged model.parsedDigit
        , demoTableInputRow "any float" Guarded.Input.Parsers.simpleFloatParser FloatChanged model.parsedFloat
        , demoTableInputRow "non-negative float" Guarded.Input.Parsers.simpleNonNegativeFloatParser NonNegativeFloatChanged model.parsedNonNegativeFloat
        , demoTableInputRow "yes/no boolean" boolParser BoolChanged model.parsedBool
        ]


demoTableInputRow : String -> (String -> Guarded.Input.Msg value) -> (Guarded.Input.Msg value -> Msg) -> Guarded.Input.Model value -> Html Msg
demoTableInputRow description inputParser msgTag parsedModel =
    tr
        []
        [ td
            []
            [ text description ]
        , td
            []
            [ lazy3 demoInput inputParser msgTag parsedModel
            ]
        , td
            []
            [ div
                [ classList <| demoClassListForInput parsedModel ]
                [ text <| (Guarded.Input.inputStringMaybe parsedModel |> Maybe.withDefault "(none)") ]
            ]
        , td
            []
            [ div
                [ classList <| demoClassListForWarning parsedModel ]
                [ text <| Maybe.withDefault "" <| Guarded.Input.lastError parsedModel ]
            ]
        ]


demoInput : (String -> Guarded.Input.Msg value) -> (Guarded.Input.Msg value -> Msg) -> Guarded.Input.Model value -> Html Msg
demoInput inputParser msgTag parsedModel =
    input
        [ Guarded.Input.parseOnInput msgTag inputParser
        , value <| Guarded.Input.inputString parsedModel
        , classList <| demoClassListForInput parsedModel
        ]
        []


demoClassPurposes : List ( String, Guarded.Input.CssUtil.Purpose )
demoClassPurposes =
    [ ( "guarded-input-valid", Guarded.Input.CssUtil.Valid )
    , ( "guarded-input-invalid", Guarded.Input.CssUtil.Invalid )
    , ( "guarded-input-work-in-progress", Guarded.Input.CssUtil.WorkInProgress )
    , ( "guarded-input-undefined", Guarded.Input.CssUtil.Undefined )
    ]


demoClassListForInput : Guarded.Input.Model value -> List ( String, Bool )
demoClassListForInput =
    Guarded.Input.CssUtil.classListForInput demoClassPurposes


demoClassListForWarning : Guarded.Input.Model value -> List ( String, Bool )
demoClassListForWarning =
    Guarded.Input.CssUtil.classListForWarning demoClassPurposes



-- CUSTOM PARSER


boolConverter : String -> Result String Bool
boolConverter input =
    case input of
        "yes" ->
            Ok True

        "no" ->
            Ok False

        _ ->
            Err <| "Cannot convert '" ++ input ++ "' to boolean."


isWorkInProgressForBool : String -> Bool
isWorkInProgressForBool input =
    if String.startsWith input "yes" || String.startsWith input "no" then
        True
    else
        False


boolParser : String -> Guarded.Input.Msg Bool
boolParser =
    Guarded.Input.parser boolConverter isWorkInProgressForBool
