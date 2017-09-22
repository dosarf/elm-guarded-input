module Demo.GuardedInputDemoApp exposing (..)

import Guarded.Input
import Guarded.Input.Parsers
import Guarded.Input.CssClasses
import Html exposing (Attribute, Html, div, input, text, table, caption, span, thead, tbody, tr, td)
import Html.Attributes exposing (class, classList, value, style)
import Html.Lazy exposing (lazy3)


type alias Model =
    { parsedAnyInt : Guarded.Input.Model Int
    , parsedNonNegativeInt : Guarded.Input.Model Int
    , parsedDigit : Guarded.Input.Model Int
    }


initialModel : Model
initialModel =
    { parsedAnyInt = Guarded.Input.initFor -42
    , parsedNonNegativeInt = Guarded.Input.initFor 42
    , parsedDigit = Guarded.Input.initFor 2
    }


type Msg
    = AnyIntChanged (Guarded.Input.Msg Int)
    | NonNegativeIntChanged (Guarded.Input.Msg Int)
    | DigitChanged (Guarded.Input.Msg Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AnyIntChanged msg ->
            let
                ( newParsedInt, subCmd ) =
                    Guarded.Input.update msg model.parsedAnyInt
            in
                ( { model
                    | parsedAnyInt = newParsedInt
                  }
                , Cmd.map AnyIntChanged subCmd
                )

        NonNegativeIntChanged msg ->
            let
                ( newParsedInt, subCmd ) =
                    Guarded.Input.update msg model.parsedNonNegativeInt
            in
                ( { model
                    | parsedNonNegativeInt = newParsedInt
                  }
                , Cmd.map NonNegativeIntChanged subCmd
                )

        DigitChanged msg ->
            let
                ( newParsedInt, subCmd ) =
                    Guarded.Input.update msg model.parsedDigit
            in
                ( { model
                    | parsedDigit = newParsedInt
                  }
                , Cmd.map DigitChanged subCmd
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
        [ demoTableInputRow "Any integer" Guarded.Input.Parsers.intParser AnyIntChanged model.parsedAnyInt
        , demoTableInputRow "Non-negative" Guarded.Input.Parsers.nonNegativeIntParser NonNegativeIntChanged model.parsedNonNegativeInt
        , demoTableInputRow "Digit" Guarded.Input.Parsers.digitParser DigitChanged model.parsedDigit
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
                [ classList <| Guarded.Input.CssClasses.defaultClassListForInput parsedModel ]
                [ text <| (Guarded.Input.inputStringMaybe parsedModel |> Maybe.withDefault "(none)") ]
            ]
        , td
            []
            [ div
                [ classList <| Guarded.Input.CssClasses.defaultClassListForAlert parsedModel ]
                [ text <| Maybe.withDefault "" <| Guarded.Input.lastError parsedModel ]
            ]
        ]


demoInput : (String -> Guarded.Input.Msg value) -> (Guarded.Input.Msg value -> Msg) -> Guarded.Input.Model value -> Html Msg
demoInput inputParser msgTag parsedModel =
    input
        [ Guarded.Input.parseOnInput msgTag inputParser
        , value (Guarded.Input.inputString parsedModel)
        ]
        []
