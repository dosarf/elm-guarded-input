module Guarded.Input.Parsers exposing
    ( intParser, nonNegativeIntParser, simpleFloatParser, simpleNonNegativeFloatParser, decimalDigitParser
    , isWorkInProgressForNumber, nothingIsWorkInProgress
    , nonNegativeNumberChecker, positiveNumberChecker, decimalDigitChecker, boundedNumberChecker
    )

{-| Couple of parsers to use with guarded input controls, plus some other bits
that may come useful when constructing your own parsers.


# Parsers

@docs intParser, nonNegativeIntParser, simpleFloatParser, simpleNonNegativeFloatParser, decimalDigitParser


# isWorkInProgress

@docs isWorkInProgressForNumber, nothingIsWorkInProgress


# Converter utilities

@docs nonNegativeNumberChecker, positiveNumberChecker, decimalDigitChecker, boundedNumberChecker

-}

import Guarded.Input exposing (..)



-- Parsers


{-| Parses integers, including negative ones.
-}
intParser : String -> Msg Int
intParser =
    parser (String.toInt >> Result.fromMaybe "Not an integer") isWorkInProgressForNumber


{-| Parses non-negative integers.
-}
nonNegativeIntParser : String -> Msg Int
nonNegativeIntParser =
    parser (String.toInt >> Result.fromMaybe "Not an integer" >> Result.andThen nonNegativeNumberChecker) nothingIsWorkInProgress


{-| Parses floats, including negative ones. No scientific notation is supported.
-}
simpleFloatParser : String -> Msg Float
simpleFloatParser =
    parser (String.toFloat >> Result.fromMaybe "Not a float") isWorkInProgressForNumber


{-| Parses non-negative floats. No scientific notation is supported.
-}
simpleNonNegativeFloatParser : String -> Msg Float
simpleNonNegativeFloatParser =
    parser (String.toFloat >> Result.fromMaybe "Not a float" >> Result.andThen nonNegativeNumberChecker) nothingIsWorkInProgress


{-| Parses a single decimal digit.
-}
decimalDigitParser : String -> Msg Int
decimalDigitParser =
    parser (String.toInt >> Result.fromMaybe "Not an integer" >> Result.andThen decimalDigitChecker) nothingIsWorkInProgress



-- isWorkInProgress


{-| Matches a single minus character ("-"). Useful for number parsers: while
a single "-" character does not yet constitutes a valid number (integer or float),
it is a valid beginning for one.
-}
isWorkInProgressForNumber : String -> Bool
isWorkInProgressForNumber input =
    "-" == input


{-| Matches no input as a work-in-progress input. Useful in cases when the
set of work-in-progress is empty (i.e. anything rejected by the convert function
is invalid).
-}
nothingIsWorkInProgress : String -> Bool
nothingIsWorkInProgress input =
    False



-- Converter utilities


{-| Use it together with either `String.toInt` or `String.toFloat` to get a converter
that converts only non-negative numbers.

To get a converter for non-negative floats:

    nonNegativeFloatConverter : String -> Result String Float
    nonNegativeFloatConverter =
        String.toFloat >> Result.fromMaybe "Not a Float" >> Result.andThen nonNegativeNumberChecker

To get a converter for non-negative integers:

    nonNegativeIntConverter : String -> Result String Int
    nonNegativeIntConverter =
        String.toInt >> Result.fromMaybe "Not an Int" >> Result.andThen nonNegativeNumberChecker

-}
nonNegativeNumberChecker : number -> Result String number
nonNegativeNumberChecker =
    boundedNumberChecker (>=) 0 "Less than 0"


{-| Use it together with either `String.toInt` or `String.toFloat` to get a converter
that converts only positive numbers.

To get a converter for positive floats:

    positiveFloatConverter : String -> Result String Float
    positiveFloatConverter =
        String.toFloat >> Result.fromMaybe "Not a Float" >> Result.andThen positiveNumberChecker

-}
positiveNumberChecker : number -> Result String number
positiveNumberChecker =
    boundedNumberChecker (>) 0 "Less or equal than 0"


{-| Use it together with either `String.toInt` to get a converter
that converts only single decimal digits.

    converter : String -> Result String Int
    converter =
        String.toInt >> Result.fromMaybe "Not an int" >> Result.andThen decimalDigitChecker

-}
decimalDigitChecker : number -> Result String number
decimalDigitChecker =
    boundedNumberChecker (>=) 0 "Less than  0" >> Result.andThen (boundedNumberChecker (<=) 9 "More than 9")


{-| Given a comparator, a boundary and an error message, it returns a checker
that can be used together with either `String.toInt` or `String.toFloat`.
Comparision expression for accepting a number `x` is: `comparator x boundary`,
e.g. `(>=) x 0` will accept numbers greater or equal to 0.

    monthIntChecker : Int -> Result String Int
    monthIntChecker =
        boundedNumberChecker (>=) 1 "Less than  1" >> Result.andThen (boundedNumberChecker (<=) 12 "More than 12")

    monthIntConverter : String -> Result String Int
    monthIntConverter =
        String.toInt >> Result.fromMaybe "Not an int" >> Result.andThen monthIntChecker

-}
boundedNumberChecker : (number -> number -> Bool) -> number -> String -> (number -> Result String number)
boundedNumberChecker comparator boundary message =
    \x ->
        if comparator x boundary then
            Ok x

        else
            Err <| message
