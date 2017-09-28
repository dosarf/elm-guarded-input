module Guarded.Input.CssUtil exposing (Purpose(..), classListForInput, classListForWarning)

{-| Support for applying different CSS classes to HTML elements based
on guarded input control state.

The model of a guarded input control actually tracks two things:
- the state of the actual input
- the info about the last erroneous input attempt, if any.

You could define CSS class for the guarded input control itself using
`classListForInput`, which then has to deal with the following states of the
input: valid, work-in-progress, undefined.

You could also define CSS class for an warning box using
`classListForWarning`, if you want to show error info messages, which then has
to deal with the following states of the last error/input state: invalid,
valid, work-in-progress, undefined.

# Types
@docs Purpose


# CSS class utilities
@docs classListForInput, classListForWarning
-}

import Guarded.Input.InternalTypes exposing (ParsedInput_(..), Model_(..))
import Guarded.Input exposing (Model)


-- Types


{-| Used to define the purpose of a CSS class. See `classListForInput` for
an example.
-}
type Purpose
    = Valid
    | Invalid
    | WorkInProgress
    | Undefined



-- CSS class utilities


{-| Given a list of (class, purpose) pairs and a model, it returns a list of
(class, bool) pairs to give directly to `Html.Attirbute.classList` for a guarded
input control. It requires a class for 3 eventualities: valid, work-in-progress
and undefined state.

    type alias Model =
        { parsedInteger : Guarded.Input.Model Int
        , ...
        }

    view : Model -> Html Msg
    view model =
        ...
        input
            [ ...
            , classList <| classListForInput myClasses model.parsedInteger
            ]
            []
        ...

    myClasses : List ( String, Guarded.Input.CssUtil.Purpose )
    myClasses =
        [ ( "guarded-input-valid", Guarded.Input.CssUtil.Valid )
        , ( "guarded-input-work-in-progress", Guarded.Input.CssUtil.WorkInProgress )
        , ( "guarded-input-undefined", Guarded.Input.CssUtil.Undefined )
        ]
-}
classListForInput : List ( String, Purpose ) -> Model value -> List ( String, Bool )
classListForInput =
    classList selectClassForInput


{-| Given a list of (class, purpose) pairs and a model, it returns a list of
(class, bool) pairs to give directly to `Html.Attirbute.classList` for a warning
box for a guarded input control. It requires a class for 4 eventualities:
valid, invalid, work-in-progress and undefined state.

    type alias Model =
        { parsedInteger : Guarded.Input.Model Int
        , ...
        }

    view : Model -> Html Msg
    view model =
        ...
        div
            [ classList <| classListForWarning myClasses model.parsedInteger ]
            [ text <| Maybe.withDefault "" <| Guarded.Input.lastError model.parsedInteger ]
        ...

    myClasses : List ( String, Guarded.Input.CssUtil.Purpose )
    myClasses =
        [ ( "guarded-input-valid", Guarded.Input.CssUtil.Valid )
        , ( "guarded-input-invalid", Guarded.Input.CssUtil.Invalid )
        , ( "guarded-input-work-in-progress", Guarded.Input.CssUtil.WorkInProgress )
        , ( "guarded-input-undefined", Guarded.Input.CssUtil.Undefined )
        ]
-}
classListForWarning : List ( String, Purpose ) -> Model value -> List ( String, Bool )
classListForWarning =
    classList selectClassForWarning



-- Implementation


classList : (Model value -> Purpose -> Bool) -> List ( String, Purpose ) -> Model value -> List ( String, Bool )
classList selector classPurposes model =
    List.map (\( class, purpose ) -> ( class, selector model purpose )) classPurposes


selectClassForInput : Model value -> Purpose -> Bool
selectClassForInput (Model_ model) purpose =
    case ( model.parsedInput, purpose ) of
        ( Valid_ _, Valid ) ->
            True

        ( WorkInProgress_ _, WorkInProgress ) ->
            True

        ( Undefined_, Undefined ) ->
            True

        _ ->
            False


selectClassForWarning : Model value -> Purpose -> Bool
selectClassForWarning (Model_ model) purpose =
    case ( model.lastError, purpose ) of
        ( Just { input, info }, Invalid ) ->
            True

        ( Just { input, info }, _ ) ->
            False

        ( Nothing, x ) ->
            selectClassForInput (Model_ model) x
