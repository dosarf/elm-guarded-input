module Guarded.Input.Parser exposing (..)

import Guarded.Util.Integer exposing (toNonNaNInt)
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input.Types exposing (..)
import Regex exposing (regex, contains)


-- PARSERS


intParser : String -> Msg Int
intParser =
    parser intGuard toNonNaNInt


nonNegativeIntParser : String -> Msg Int
nonNegativeIntParser =
    parser nonNegativeIntGuard (toNonNaNInt >> Result.andThen nonNegativeNumberConverter)


simpleFloatParser : String -> Msg Float
simpleFloatParser =
    parser simpleFloatGuard String.toFloat


simpleNonNegativeFloatParser : String -> Msg Float
simpleNonNegativeFloatParser =
    parser simpleNonNegativeFloatGuard (String.toFloat >> Result.andThen nonNegativeNumberConverter)


digitParser : String -> Msg Int
digitParser =
    parser digitGuard (toNonNaNInt >> Result.andThen digitConverter)



-- TODO comment guard: accepts for feasible (work-in-progress or valid) input (-> Nothing), rejects all else (-> Just error)
-- TODO comment convert: converts all valid text to valid value (or fails)
-- TODO comment: any input that wasn't rejected by the guard but failed to convert is considered WiP input


parser : (String -> Maybe String) -> (String -> Result String value) -> String -> Msg value
parser guard convert input =
    if input == "" then
        Undefined_
    else
        let
            rejectionResult =
                guard input

            conversionResult =
                convert input
        in
            case ( rejectionResult, conversionResult ) of
                ( Just error, _ ) ->
                    Invalid_ ( input, error )

                ( Nothing, Err error ) ->
                    WorkInProgress_ ( input, error )

                ( Nothing, Ok v ) ->
                    Valid_ v



-- GUARDS


regexGuard : Regex.Regex -> String -> String -> Maybe String
regexGuard regex errorMessage input =
    let
        matches =
            Regex.contains regex input
    in
        if matches then
            Nothing
        else
            Just <| errorMessage ++ ": " ++ input


intGuard : String -> Maybe String
intGuard =
    regexGuard (Regex.regex "^-?\\d*$") "Cannot be completed to an integer"


nonNegativeIntGuard : String -> Maybe String
nonNegativeIntGuard =
    regexGuard (Regex.regex "^\\d*$") "Cannot be completed to a non-negative integer"


simpleFloatGuard : String -> Maybe String
simpleFloatGuard =
    regexGuard (Regex.regex "^-?\\d*(\\.\\d*)?$") "Cannot be completed to a float"


simpleNonNegativeFloatGuard : String -> Maybe String
simpleNonNegativeFloatGuard =
    regexGuard (Regex.regex "^\\d*(\\.\\d*)?$") "Cannot be completed to a non-negative float"


digitGuard : String -> Maybe String
digitGuard =
    regexGuard (Regex.regex "^\\d?$") "Cannot be completed to a digit"



-- CONVERTERS


nonNegativeNumberConverter : comparable -> Result String comparable
nonNegativeNumberConverter =
    boundedNumberConverter (>=) 0 "Less than 0"


positiveNumberConverter : comparable -> Result String comparable
positiveNumberConverter =
    boundedNumberConverter (>) 0 "Less or equal than 0"


digitConverter : comparable -> Result String comparable
digitConverter =
    boundedNumberConverter (>=) 0 "Less than  0" >> Result.andThen (boundedNumberConverter (<=) 9 "More than 9")



-- TODO comment: comparision invocation order: comparator value boundary


boundedNumberConverter : (comparable -> comparable -> Bool) -> comparable -> String -> (comparable -> Result String comparable)
boundedNumberConverter comparator boundary message =
    \x ->
        if comparator x boundary then
            Ok x
        else
            Err <| message ++ ": " ++ (toString x)
