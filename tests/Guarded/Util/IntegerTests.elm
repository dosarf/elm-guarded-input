module Guarded.Util.IntegerTests exposing (testSuite)

import Guarded.Util.Integer exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (int, string)


testSuite : Test
testSuite =
    describe "Guarded.Util.Integer tests"
        [ describe "intIsNaN tests"
            [ fuzz int "Actual integers are not NaN" <|
                \x ->
                    False
                        |> Expect.equal (intIsNaN x)
            , test "A minus ('-') sign converted to Int is NaN" <|
                \() ->
                    Expect.true "String.toInt '-' is NaN" (String.toInt "-" |> Result.toMaybe |> Maybe.withDefault 0 |> intIsNaN)
            ]
        , describe "nonNaNIntResult tests"
            [ fuzz int "Actual integers are not NaN" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNaNIntResult x)
            , test "A minus ('-') sign converted to Int is NaN" <|
                \() ->
                    Err "NaN"
                        |> Expect.equal (String.toInt "-" |> Result.andThen nonNaNIntResult)
            ]
        , describe "toNonNanInt tests"
            [ fuzz int "Valid integers are converted" <|
                \x ->
                    Ok x
                        |> Expect.equal (toNonNaNInt <| toString x)
            , test "A minus ('-') sign is not converted to integer (not matter what String.toInt thinks)" <|
                \() ->
                    Err "NaN"
                        |> Expect.equal (toNonNaNInt "-")
            , fuzz string "Garbage input is not accepted as valid integer" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        True
                            |> Expect.equal (toNonNaNInt garbledInput |> resultContainsError garbledInput)
            ]
        ]


resultContainsError : String -> Result String value -> Bool
resultContainsError subString result =
    case result of
        Ok _ ->
            False

        Err error ->
            String.contains subString error
