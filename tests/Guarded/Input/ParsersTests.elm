module Guarded.Input.ParsersTests exposing (testSuite)

import Expect
import Fuzz exposing (float, floatRange, int, intRange, string)
import Guarded.Input exposing (..)
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input.Parsers exposing (..)
import String
import Test exposing (..)


testSuite : Test
testSuite =
    describe "Guarded.Input.Parser tests"
        [ isWorkInProgressTestSuite
        , checkerTestSuite
        , parserTestSuite
        ]


isWorkInProgressTestSuite : Test
isWorkInProgressTestSuite =
    describe "isWorkInProgress tests"
        [ describe "isWorkInProgressForNumber tests"
            [ test "A single minus char string is accepted as work-in-progress" <|
                \() ->
                    Expect.true "minus" (isWorkInProgressForNumber "-")
            , test "An empty string is not accepted as work-in-progress" <|
                \() ->
                    Expect.false "empty" (isWorkInProgressForNumber "")
            , test "Strings (other than a minus char) are not accepted as work-in-progress" <|
                \() ->
                    Expect.false "some other string" (isWorkInProgressForNumber "12a")
            ]
        , describe "nothingIsWorkInProgress tests"
            [ fuzz string "no string is accepted as work-in-progress" <|
                \x ->
                    Expect.false "any string" (nothingIsWorkInProgress x)
            ]
        ]


checkerTestSuite : Test
checkerTestSuite =
    describe "Checker tests"
        [ describe "nonNegativeNumberChecker tests for integers"
            [ fuzz (intRange 0 100) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNegativeNumberChecker x)
            , fuzz (intRange 1 100) "fails negative" <|
                \x ->
                    Expect.true "negative" (nonNegativeNumberChecker -x |> errorResultContainsString "Less than 0")
            ]
        , describe "nonNegativeNumberChecker tests for float"
            [ fuzz (floatRange 0.0 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNegativeNumberChecker x)
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (nonNegativeNumberChecker -x |> errorResultContainsString "Less than 0")
            ]
        , describe "positiveNumberChecker tests for integers"
            [ fuzz (intRange 1 100) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberChecker x)
            , test "fails 0" <|
                \() ->
                    Expect.true "zero" (positiveNumberChecker 0 |> errorResultContainsString "Less or equal than 0")
            , fuzz (intRange 1 100) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberChecker -x |> errorResultContainsString "Less or equal than 0")
            ]
        , describe "positiveNumberChecker tests for floats"
            [ fuzz (floatRange 0.01 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberChecker x)
            , test "fails 0.0" <|
                \() ->
                    Expect.true "zero" (positiveNumberChecker 0.0 |> errorResultContainsString "Less or equal than 0")
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberChecker -x |> errorResultContainsString "Less or equal than 0")
            ]
        ]


parserTestSuite : Test
parserTestSuite =
    describe "type specific parser tests"
        [ describe "intParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    UndefinedMsg_
                        |> Expect.equal (intParser "")
            , test "A single minus character yields work-in-progress message" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (intParser "-" |> workInProgressMsgInput)
            , fuzz int "A valid integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( x, String.fromInt x )
                        |> Expect.equal (intParser (String.fromInt x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0, "0" )
                        |> Expect.equal (intParser "0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            "1a3" ++ x
                    in
                    Ok garbledInput
                        |> Expect.equal (intParser garbledInput |> invalidMsgInput)
            ]
        , describe "nonNegativeIntParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    UndefinedMsg_
                        |> Expect.equal (nonNegativeIntParser "")
            , test "A single minus character is invalid" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (nonNegativeIntParser "-" |> invalidMsgInput)
            , fuzz (intRange 0 1234) "A valid non-negative integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( x, String.fromInt x )
                        |> Expect.equal (nonNegativeIntParser (String.fromInt x))
            , test "Zero yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0, "0" )
                        |> Expect.equal (nonNegativeIntParser "0")
            , test "A negative integer yields an invalid messsage" <|
                \() ->
                    Ok "-123"
                        |> Expect.equal (nonNegativeIntParser "-123" |> invalidMsgInput)
            , fuzz string "Garbage yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            "1a3" ++ x
                    in
                    Ok garbledInput
                        |> Expect.equal (nonNegativeIntParser garbledInput |> invalidMsgInput)
            ]
        , describe "simpleFloatParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    UndefinedMsg_
                        |> Expect.equal (simpleFloatParser "")
            , test "A single minus character yields work-in-progress message" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (simpleFloatParser "-" |> workInProgressMsgInput)
            , fuzz (intRange 1 100) "A valid positive integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( toFloat x, toFloat x |> String.fromFloat )
                        |> Expect.equal (simpleFloatParser (String.fromInt x))
            , fuzz (intRange 1 100) "A valid negative integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( toFloat -x, toFloat -x |> String.fromFloat )
                        |> Expect.equal (simpleFloatParser (String.fromInt -x))
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( x, String.fromFloat x )
                        |> Expect.equal (simpleFloatParser (String.fromFloat x))
            , fuzz (floatRange 0.01 100.0) "A negative float yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( -x, String.fromFloat -x )
                        |> Expect.equal (simpleFloatParser (String.fromFloat -x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0, "0" )
                        |> Expect.equal (simpleFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0.0, "0.0" )
                        |> Expect.equal (simpleFloatParser "0.0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            "1a3" ++ x
                    in
                    Ok garbledInput
                        |> Expect.equal (simpleFloatParser garbledInput |> invalidMsgInput)
            ]
        , describe "simpleNonNegativeFloatParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    UndefinedMsg_
                        |> Expect.equal (simpleNonNegativeFloatParser "")
            , test "A single minus character is rejected" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (simpleNonNegativeFloatParser "-" |> invalidMsgInput)
            , fuzz (intRange 1 100) "A valid positive integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( toFloat x, toFloat x |> String.fromFloat )
                        |> Expect.equal (simpleNonNegativeFloatParser <| String.fromInt x)
            , fuzz (intRange 1 100) "A valid negative integer yields an invalid messsage" <|
                \x ->
                    Ok (String.fromInt -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (String.fromInt -x) |> invalidMsgInput)
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    ValidMsg_ ( x, String.fromFloat x )
                        |> Expect.equal (simpleNonNegativeFloatParser <| String.fromFloat x)
            , fuzz (floatRange 0.01 100.0) "A negative float yields an invalid messsage" <|
                \x ->
                    Ok (String.fromFloat -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (String.fromFloat -x) |> invalidMsgInput)
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0, "0" )
                        |> Expect.equal (simpleNonNegativeFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ ( 0.0, "0.0" )
                        |> Expect.equal (simpleNonNegativeFloatParser "0.0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            "1a3" ++ x
                    in
                    Ok garbledInput
                        |> Expect.equal (simpleNonNegativeFloatParser garbledInput |> invalidMsgInput)
            ]
        , describe "decimalDigitParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    UndefinedMsg_
                        |> Expect.equal (decimalDigitParser "")
            , test "A single minus character is rejected" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (decimalDigitParser "-" |> invalidMsgInput)
            , fuzz (intRange 0 9) "Decimal decimalDigits are accpeted" <|
                \x ->
                    ValidMsg_ ( x, String.fromInt x )
                        |> Expect.equal (decimalDigitParser <| String.fromInt x)
            , fuzz (intRange 10 100) "A positive integer >9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            String.fromInt x
                    in
                    Ok input
                        |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , fuzz (intRange 1 100) "A negative integer yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            String.fromInt -x
                    in
                    Ok input
                        |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , test "A float between 0 and 9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            String.fromFloat 1.23
                    in
                    Ok input
                        |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            "1a3" ++ x
                    in
                    Ok garbledInput
                        |> Expect.equal (decimalDigitParser garbledInput |> invalidMsgInput)
            ]
        ]


errorContains : String -> Maybe String -> Bool
errorContains subString maybeError =
    case maybeError of
        Nothing ->
            False

        Just error ->
            String.contains subString error


errorResultContainsString : String -> Result String value -> Bool
errorResultContainsString subString result =
    case result of
        Ok _ ->
            False

        Err error ->
            String.contains subString error


workInProgressMsgInput : Msg v -> Result String String
workInProgressMsgInput msg =
    case msg of
        UndefinedMsg_ ->
            Err "undefined"

        WorkInProgressMsg_ ( input, _ ) ->
            Ok input

        ValidMsg_ _ ->
            Err "valid"

        InvalidMsg_ _ ->
            Err "invalid"


invalidMsgInput : Msg v -> Result String String
invalidMsgInput msg =
    case msg of
        UndefinedMsg_ ->
            Err "undefined"

        WorkInProgressMsg_ _ ->
            Err "work-in-progress"

        ValidMsg_ _ ->
            Err "valid"

        InvalidMsg_ ( input, _ ) ->
            Ok input


resultContainsError : String -> Result String value -> Bool
resultContainsError subString result =
    case result of
        Ok _ ->
            False

        Err error ->
            String.contains subString error
