module Guarded.Util.IntegerTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (int, string)
import Guarded.Util.Integer exposing (..)


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
        ]
