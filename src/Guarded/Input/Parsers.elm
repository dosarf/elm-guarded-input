module Guarded.Input.Parsers exposing (..)

{-| Couple of parsers to use with guarded input controls, plus some other bits
that may come useful when constructing your own parsers.

# Parsers
@docs intParser, nonNegativeIntParser, simpleFloatParser, simpleNonNegativeFloatParser, decimalDigitParser

# isWorkInProgress
@docs isWorkInProgressForNegativeNumber, nothingIsWorkInProgress

# Converters
@docs intConverter

# Converter utilities
@docs nonNegativeNumberChecker, positiveNumberChecker, decimalDigitChecker, boundedNumberChecker
-}

import Guarded.Util.Integer exposing (..)
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input exposing (..)


-- Parsers


{-| Parses integers, including negative ones.
-}
intParser : String -> Msg Int
intParser =
    parser intConverter isWorkInProgressForNegativeNumber


{-| Parses non-negative integers.
-}
nonNegativeIntParser : String -> Msg Int
nonNegativeIntParser =
    parser (intConverter >> Result.andThen nonNegativeNumberChecker) nothingIsWorkInProgress


{-| Parses floats, including negative ones.
-}
simpleFloatParser : String -> Msg Float
simpleFloatParser =
    parser String.toFloat isWorkInProgressForNegativeNumber


{-| Parses non-negative floats.
-}
simpleNonNegativeFloatParser : String -> Msg Float
simpleNonNegativeFloatParser =
    parser (String.toFloat >> Result.andThen nonNegativeNumberChecker) nothingIsWorkInProgress


{-| Parses a single decimal digit.
-}
decimalDigitParser : String -> Msg Int
decimalDigitParser =
    parser (intConverter >> Result.andThen decimalDigitChecker) nothingIsWorkInProgress



-- isWorkInProgress


{-| Matches a single minus character ("-"). Useful for number parsers: while
a single "-" character does not yet constitutes a valid integer or float,
it is a valid beginning for a negative number.
-}
isWorkInProgressForNegativeNumber : String -> Bool
isWorkInProgressForNegativeNumber input =
    "-" == input


{-| Matches no input as a work-in-progress input. Useful in cases when the
set of work-in-progress is empty, like when matching positive integers.
-}
nothingIsWorkInProgress : String -> Bool
nothingIsWorkInProgress input =
    False



-- Converters


{-| A String -> String.toInt will return a buggy `NaN` for a "-" (single
minus character). This workaround function will return a proper Err for it.

    intConverter "123" -- 123
    intConverter "-123" -- -123
    intConverter "a" -- Err "could not convert string 'a' to an Int"
    intConverter "-" -- Err "NaN"
-}
intConverter : String -> Result String Int
intConverter =
    String.toInt
        >> Result.andThen nonNaNIntResult



-- Converter utilities


{-| Use it together with either `intConverter` or `String.toFloat` to get a converter
that converts only non-negative numbers.

To get a converter for non-negative floats:

    nonNegativeFloatConverter : String -> Result String Float
    nonNegativeFloatConverter = String.toFloat >> Result.andThen nonNegativeNumberChecker

To get a converter for non-negative integers:

    nonNegativeIntConverter : String -> Result String Int
    nonNegativeIntConverter = intConverter >> Result.andThen nonNegativeNumberChecker

See `intConverter` as to why prefer that instead of `String.toInt`.
-}
nonNegativeNumberChecker : comparable -> Result String comparable
nonNegativeNumberChecker =
    boundedNumberChecker (>=) 0 "Less than 0"


{-| Use it together with either `intConverter` or `String.toFloat` to get a converter
that converts only positive numbers.

To get a converter for positive floats:

    positiveFloatConverter : String -> Result String Float
    positiveFloatConverter = String.toFloat >> Result.andThen positiveNumberChecker
-}
positiveNumberChecker : comparable -> Result String comparable
positiveNumberChecker =
    boundedNumberChecker (>) 0 "Less or equal than 0"


{-| Use it together with either `intConverter` to get a converter
that converts only single decimal digits.

    converter : String -> Result String Int
    converter = intConverter >> Result.andThen decimalDigitChecker

See `intConverter` as to why prefer that instead of `String.toInt`.
-}
decimalDigitChecker : comparable -> Result String comparable
decimalDigitChecker =
    boundedNumberChecker (>=) 0 "Less than  0" >> Result.andThen (boundedNumberChecker (<=) 9 "More than 9")


{-| Given a comparator, a boundary and an error message, it returns a checker
that can be used together with either `intConverter` or `String.toFloat`.
Comparision expression for accepting a number `x` is: `comparator x boundary`,
e.g. `(>=) x 0` will accept numbers greater or equal to 0.

    monthIntChecker : Int -> Result String Int
    monthIntChecker = boundedNumberChecker (>=) 1 "Less than  1" >> Result.andThen (boundedNumberChecker (<=) 12 "More than 12")

    monthIntConverter : String -> Result String Int
    monthIntConverter = intConverter >> Result.andThen monthIntChecker
-}
boundedNumberChecker : (comparable -> comparable -> Bool) -> comparable -> String -> (comparable -> Result String comparable)
boundedNumberChecker comparator boundary message =
    \x ->
        if comparator x boundary then
            Ok x
        else
            Err <| message ++ ": " ++ (toString x)
