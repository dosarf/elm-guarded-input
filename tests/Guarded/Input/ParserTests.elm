module Guarded.Input.ParserTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (float, floatRange, int, intRange, string)
import String
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input.Types exposing (..)
import Guarded.Input.Parser exposing (..)


-- TODO boundedNumberConverter tests
-- TODO create custom string-list guard parser


testSuite : Test
testSuite =
    describe "Guarded.Input.Parser tests"
        [ guardTestSuite
        , converterTestSuite
        , parserTestSuite
        ]


guardTestSuite : Test
guardTestSuite =
    describe "guard tests"
        [ describe "intGuard tests"
            [ test "accepts '-'" <|
                \() ->
                    Nothing
                        |> Expect.equal (intGuard "-")
            , fuzz (intRange 1 100) "accepts negative integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (intGuard <| toString -x)
            , fuzz (intRange 0 100) "accepts positive integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (intGuard <| toString x)
            , fuzz string "rejects garbage" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        Expect.true "rejects garbage" (intGuard garbledInput |> errorContains garbledInput)
            ]
        , describe "nonNegativeIntGuard"
            [ test "rejects '-'" <|
                \() ->
                    Expect.true "'-'" (nonNegativeIntGuard "-" |> errorContains "-")
            , fuzz (intRange 1 100) "accepts negative integers" <|
                \x ->
                    let
                        negativeInput =
                            "-" ++ (toString x)
                    in
                        Expect.true "negative" (nonNegativeIntGuard negativeInput |> errorContains negativeInput)
            , fuzz (intRange 0 100) "accepts positive integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (nonNegativeIntGuard <| toString x)
            , fuzz string "rejects garbage" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        Expect.true "garbage" (nonNegativeIntGuard garbledInput |> errorContains garbledInput)
            ]
        , describe "simpleFloatGuard"
            [ test "accepts '-'" <|
                \() ->
                    Nothing
                        |> Expect.equal (simpleFloatGuard "-")
            , fuzz (intRange 1 100) "accepts negative integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleFloatGuard <| toString -x)
            , fuzz (floatRange 0.1 100.0) "accepts negative floats" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleFloatGuard <| toString -x)
            , fuzz (intRange 0 100) "accepts positive integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleFloatGuard <| toString x)
            , fuzz (floatRange 0.0 100.0) "accepts positive floats" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleFloatGuard <| toString x)
            , fuzz string "rejects garbage" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        Expect.true "garbage" (simpleFloatGuard garbledInput |> errorContains garbledInput)
            ]
        , describe "simpleNonNegativeFloatGuard"
            [ test "rejects '-'" <|
                \() ->
                    Expect.true "-" (simpleNonNegativeFloatGuard "-" |> errorContains "-")
            , fuzz (intRange 1 100) "rejects negative integers" <|
                \x ->
                    let
                        input =
                            toString -x
                    in
                        Expect.true "negative integer" (simpleNonNegativeFloatGuard input |> errorContains input)
            , fuzz (floatRange 0.1 100.0) "rejects negative floats" <|
                \x ->
                    let
                        input =
                            toString -x
                    in
                        Expect.true "negative float" (simpleNonNegativeFloatGuard input |> errorContains input)
            , fuzz (intRange 0 100) "accepts positive integers" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleNonNegativeFloatGuard <| toString x)
            , fuzz (floatRange 0.0 100.0) "accepts positive floats" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleNonNegativeFloatGuard <| toString x)
            , fuzz string "rejects garbage" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        Expect.true "garbage" (simpleNonNegativeFloatGuard garbledInput |> errorContains garbledInput)
            ]
        , describe "digitGuard tests"
            [ test "rejects '-'" <|
                \() ->
                    Expect.true "-" (digitGuard "-" |> errorContains "-")
            , fuzz (intRange 1 100) "rejects negative integers" <|
                \x ->
                    let
                        input =
                            toString -x
                    in
                        Expect.true "negative integer" (digitGuard input |> errorContains input)
            , fuzz (intRange 0 9) "accepts positive digits" <|
                \x ->
                    Nothing
                        |> Expect.equal (simpleNonNegativeFloatGuard <| toString x)
            , fuzz (intRange 10 100) "rejects positive integers > 9" <|
                \x ->
                    let
                        input =
                            toString x
                    in
                        Expect.true "too big integer" (digitGuard input |> errorContains input)
            , fuzz string "rejects garbage" <|
                \x ->
                    let
                        garbledInput =
                            "a" ++ x
                    in
                        Expect.true "garbage" (digitGuard garbledInput |> errorContains garbledInput)
            ]
        ]


converterTestSuite : Test
converterTestSuite =
    describe "converter tests"
        [ describe "nonNegativeNumberConverter tests for integers"
            [ fuzz (intRange 0 100) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNegativeNumberConverter x)
            , fuzz (intRange 1 100) "fails negative" <|
                \x ->
                    Expect.true "negative" (nonNegativeNumberConverter -x |> errorResultContainsString (toString -x))
            ]
        , describe "nonNegativeNumberConverter tests for float"
            [ fuzz (floatRange 0.0 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (nonNegativeNumberConverter x)
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (nonNegativeNumberConverter -x |> errorResultContainsString (toString -x))
            ]
        , describe "positiveNumberConverter tests for integers"
            [ fuzz (intRange 1 100) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberConverter x)
            , test "fails 0" <|
                \() ->
                    Expect.true "zero" (positiveNumberConverter 0 |> errorResultContainsString "0")
            , fuzz (intRange 1 100) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberConverter -x |> errorResultContainsString (toString -x))
            ]
        , describe "positiveNumberConverter tests for floats"
            [ fuzz (floatRange 0.01 100.0) "converts non-negative" <|
                \x ->
                    Ok x
                        |> Expect.equal (positiveNumberConverter x)
            , test "fails 0.0" <|
                \() ->
                    Expect.true "zero" (positiveNumberConverter 0.0 |> errorResultContainsString "0")
            , fuzz (floatRange 0.01 100.0) "fails negative" <|
                \x ->
                    Expect.true "negative" (positiveNumberConverter -x |> errorResultContainsString (toString -x))
            ]
        ]


parserTestSuite : Test
parserTestSuite =
    describe "parser tests"
        [ describe "intParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    Undefined_
                        |> Expect.equal (intParser "")
            , test "A single minus character yields work-in-progress message" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (intParser "-" |> workInProgressMsgInput)
            , fuzz int "A valid integer yields a valid messsage" <|
                \x ->
                    Valid_ x
                        |> Expect.equal (intParser (toString x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    Valid_ 0
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
                    Undefined_
                        |> Expect.equal (nonNegativeIntParser "")
            , test "A single minus character is invalid" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (nonNegativeIntParser "-" |> invalidMsgInput)
            , fuzz (intRange 0 1234) "A valid non-negative integer yields a valid messsage" <|
                \x ->
                    Valid_ x
                        |> Expect.equal (nonNegativeIntParser <| toString x)
            , test "Zero yields a valid messsage" <|
                \() ->
                    Valid_ 0
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
                    Undefined_
                        |> Expect.equal (simpleFloatParser "")
            , test "A single minus character yields work-in-progress message" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (simpleFloatParser "-" |> workInProgressMsgInput)
            , fuzz (intRange 1 100) "A valid positive integer yields a valid messsage" <|
                \x ->
                    Valid_ (toFloat x)
                        |> Expect.equal (simpleFloatParser (toString x))
            , fuzz (intRange 1 100) "A valid negative integer yields a valid messsage" <|
                \x ->
                    Valid_ (toFloat -x)
                        |> Expect.equal (simpleFloatParser (toString -x))
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    Valid_ x
                        |> Expect.equal (simpleFloatParser (toString x))
            , fuzz (floatRange 0.01 100.0) "A negative float yields a valid messsage" <|
                \x ->
                    Valid_ -x
                        |> Expect.equal (simpleFloatParser (toString -x))
            , test "A 0 yields a valid messsage" <|
                \() ->
                    Valid_ 0.0
                        |> Expect.equal (simpleFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    Valid_ 0.0
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
                    Undefined_
                        |> Expect.equal (simpleNonNegativeFloatParser "")
            , test "A single minus character is rejected" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (simpleNonNegativeFloatParser "-" |> invalidMsgInput)
            , fuzz (intRange 1 100) "A valid positive integer yields a valid messsage" <|
                \x ->
                    Valid_ (toFloat x)
                        |> Expect.equal (simpleNonNegativeFloatParser <| toString x)
            , fuzz (intRange 1 100) "A valid negative integer yields an invalid messsage" <|
                \x ->
                    Ok (toString -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (toString -x) |> invalidMsgInput)
            , fuzz (floatRange 0.01 100.0) "A positive float yields a valid messsage" <|
                \x ->
                    Valid_ x
                        |> Expect.equal (simpleNonNegativeFloatParser <| toString x)
            , fuzz (floatRange 0.01 100.0) "A negative float yields an invalid messsage" <|
                \x ->
                    Ok (toString -x)
                        |> Expect.equal (simpleNonNegativeFloatParser (toString -x) |> invalidMsgInput)
            , test "A 0 yields a valid messsage" <|
                \() ->
                    Valid_ 0.0
                        |> Expect.equal (simpleNonNegativeFloatParser "0")
            , test "A 0.0 yields a valid messsage" <|
                \() ->
                    Valid_ 0.0
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
        , describe "digitParser tests"
            [ test "Empty string yields undefined message" <|
                \() ->
                    Undefined_
                        |> Expect.equal (digitParser "")
            , test "A single minus character is rejected" <|
                \() ->
                    Ok "-"
                        |> Expect.equal (digitParser "-" |> invalidMsgInput)
            , fuzz (intRange 0 9) "Decimal digits are accpeted" <|
                \x ->
                    Valid_ x
                        |> Expect.equal (digitParser <| toString x)
            , fuzz (intRange 10 100) "A positive integer >9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString x
                    in
                        Ok input
                            |> Expect.equal (digitParser input |> invalidMsgInput)
            , fuzz (intRange 1 100) "A negative integer yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString -x
                    in
                        Ok input
                            |> Expect.equal (digitParser input |> invalidMsgInput)
            , test "A float between 0 and 9 yields an invalid messsage" <|
                \x ->
                    let
                        input =
                            toString 1.23
                    in
                        Ok input
                            |> Expect.equal (digitParser input |> invalidMsgInput)
            , fuzz string "Garbage input yields an invalid message" <|
                \x ->
                    let
                        garbledInput =
                            ("1a3" ++ x)
                    in
                        Ok garbledInput
                            |> Expect.equal (digitParser garbledInput |> invalidMsgInput)
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
        Undefined_ ->
            Err "undefined"

        WorkInProgress_ ( input, _ ) ->
            Ok input

        Valid_ _ ->
            Err "valid"

        Invalid_ _ ->
            Err "invalid"


invalidMsgInput : Msg v -> Result String String
invalidMsgInput msg =
    case msg of
        Undefined_ ->
            Err "undefined"

        WorkInProgress_ _ ->
            Err "work-in-progress"

        Valid_ _ ->
            Err "valid"

        Invalid_ ( input, _ ) ->
            Ok input
