module Guarded.Input.ParsersTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (float, floatRange, int, intRange, string)
import String
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input exposing (..)
import Guarded.Input.Parsers exposing (..)


-- TODO boundedNumberConverter tests
-- TODO create custom string-list guard parser


testSuite : Test
testSuite =
    describe "Guarded.Input.Parser tests"
        [ isWorkInProgressTestSuite
        , converterTestSuite
        , checkerTestSuite
        , parserTestSuite
        ]


isWorkInProgressTestSuite : Test
isWorkInProgressTestSuite =
    describe "isWorkInProgress tests"
        [ describe "isWorkInProgressForNegativeNumber tests"
            [ test "A single minus char string is accepted as work-in-progress" <|
                \() ->
                    Expect.true "minus" (isWorkInProgressForNegativeNumber "-")
            , test "An empty string is not accepted as work-in-progress" <|
                \() ->
                    Expect.false "empty" (isWorkInProgressForNegativeNumber "")
            , test "Strings (other than a minus char) are not accepted as work-in-progress" <|
                \() ->
                    Expect.false "some other string" (isWorkInProgressForNegativeNumber "12a")
            ]
        , describe "nothingIsWorkInProgress tests"
            [ fuzz string "no string is accepted as work-in-progress" <|
                \x ->
                    Expect.false "any string" (nothingIsWorkInProgress x)
            ]
        ]


converterTestSuite : Test
converterTestSuite =
    describe "converter tests"
        [ describe "intConverter tests"
            [ fuzz int "Valid integers are converted" <|
                \x ->
                    Ok x
                        |> Expect.equal (intConverter <| toString x)
            , test "A minus ('-') sign is not converted to integer (not matter what String.toInt thinks)" <|
                \() ->
                    Err "NaN"
                        |> Expect.equal (intConverter "-")
            , fuzz string "Garbage input is not accepted as valid integer" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        True
                            |> Expect.equal (intConverter garbledInput |> resultContainsError garbledInput)
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
                    Expect.true "negative" (nonNegativeNumberChecker -x |> errorResultContainsString (toString -x))
            ]
        , describe "nonNegativeNumberChecker tests for float"
            [ fuzz (floatRange 0.0 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNegativeNumberChecker x)
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (nonNegativeNumberChecker -x |> errorResultContainsString (toString -x))
            ]
        , describe "positiveNumberChecker tests for integers"
            [ fuzz (intRange 1 100) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberChecker x)
            , test "fails 0" <|
                \() ->
                    Expect.true "zero" (positiveNumberChecker 0 |> errorResultContainsString "0")
            , fuzz (intRange 1 100) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberChecker -x |> errorResultContainsString (toString -x))
            ]
        , describe "positiveNumberChecker tests for floats"
            [ fuzz (floatRange 0.01 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberChecker x)
            , test "fails 0.0" <|
                \() ->
                    Expect.true "zero" (positiveNumberChecker 0.0 |> errorResultContainsString "0")
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberChecker -x |> errorResultContainsString (toString -x))
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
                    ValidMsg_ x
                        |> Expect.equal (intParser (toString x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0
                        |> Expect.equal (intParser "0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
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
                    ValidMsg_ x
                        |> Expect.equal (nonNegativeIntParser <| toString x)
            , test "Zero yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0
                        |> Expect.equal (nonNegativeIntParser "0")
            , test "A negative integer yields an invalid messsage" <|
                \() ->
                    Ok "-123"
                        |> Expect.equal (nonNegativeIntParser "-123" |> invalidMsgInput)
            , fuzz string "Garbage yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
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
                    ValidMsg_ (toFloat x)
                        |> Expect.equal (simpleFloatParser (toString x))
            , fuzz (intRange 1 100) "A valid negative integer yields a valid messsage" <|
                \x ->
                    ValidMsg_ (toFloat -x)
                        |> Expect.equal (simpleFloatParser (toString -x))
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    ValidMsg_ x
                        |> Expect.equal (simpleFloatParser (toString x))
            , fuzz (floatRange 0.01 100.0) "A negative float yields a valid messsage" <|
                \x ->
                    ValidMsg_ -x
                        |> Expect.equal (simpleFloatParser (toString -x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0.0
                        |> Expect.equal (simpleFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0.0
                        |> Expect.equal (simpleFloatParser "0.0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
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
                    ValidMsg_ (toFloat x)
                        |> Expect.equal (simpleNonNegativeFloatParser <| toString x)
            , fuzz (intRange 1 100) "A valid negative integer yields an invalid messsage" <|
                \x ->
                    Ok (toString -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (toString -x) |> invalidMsgInput)
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    ValidMsg_ x
                        |> Expect.equal (simpleNonNegativeFloatParser <| toString x)
            , fuzz (floatRange 0.01 100.0) "A negative float yields an invalid messsage" <|
                \x ->
                    Ok (toString -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (toString -x) |> invalidMsgInput)
            , test "A 0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0.0
                        |> Expect.equal (simpleNonNegativeFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    ValidMsg_ 0.0
                        |> Expect.equal (simpleNonNegativeFloatParser "0.0")
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
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
                    ValidMsg_ x
                        |> Expect.equal (decimalDigitParser <| toString x)
            , fuzz (intRange 10 100) "A positive integer >9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString x
                    in
                        Ok input
                            |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , fuzz (intRange 1 100) "A negative integer yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString -x
                    in
                        Ok input
                            |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , test "A float between 0 and 9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString 1.23
                    in
                        Ok input
                            |> Expect.equal (decimalDigitParser input |> invalidMsgInput)
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
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
