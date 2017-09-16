module Guarded.Input.CssClasses exposing (..)

import Guarded.Input.Types exposing (Model(..))
import Guarded.Input.InternalTypes exposing (ParsedInput(..))


inputValidClass : String
inputValidClass =
    "guarded-input-valid"


inputUndefinedClass : String
inputUndefinedClass =
    "guarded-input-undefined"


inputInvalidClass : String
inputInvalidClass =
    "guarded-input-invalid"


inputWorkInProgressClass : String
inputWorkInProgressClass =
    "guarded-input-work-in-progress"


classList : (Model value -> String -> Bool) -> List String -> Model value -> List ( String, Bool )
classList selectClass classes model =
    List.map (\class -> ( class, selectClass model class )) classes


defaultClassListForInput : Model value -> List ( String, Bool )
defaultClassListForInput model =
    classList selectClassForInput defaultClasses model


defaultClassListForAlert : Model value -> List ( String, Bool )
defaultClassListForAlert model =
    classList selectClassForAlert defaultClasses model


selectClassForInput : Model value -> String -> Bool
selectClassForInput (Model model) class =
    case model.parsedInput of
        Valid _ ->
            class == inputValidClass

        WorkInProgress _ ->
            class == inputWorkInProgressClass

        Undefined ->
            class == inputUndefinedClass


selectClassForAlert : Model value -> String -> Bool
selectClassForAlert (Model model) class =
    case model.lastError of
        Just { input, info } ->
            case model.parsedInput of
                WorkInProgress _ ->
                    class == inputInvalidClass

                Undefined ->
                    class == inputUndefinedClass

                _ ->
                    class == inputInvalidClass

        Nothing ->
            selectClassForInput (Model model) class


defaultClasses : List String
defaultClasses =
    [ inputValidClass
    , inputWorkInProgressClass
    , inputInvalidClass
    , inputUndefinedClass
    ]
