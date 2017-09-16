module Guarded.Input.TypesTests exposing (testSuite)

import Test exposing (..)
import Expect
import String
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input.Types exposing (..)


validModelNoLastError : Model Int
validModelNoLastError =
    fromParsedInput <| Valid 5


validModelWithLastError : Model Int
validModelWithLastError =
    fromParsedInputAndLastError (Valid 5) { input = "5a", info = "parse error" }


workInProgress : Model Int
workInProgress =
    fromParsedInput <| WorkInProgress "-"


undefinedNoLastError : Model Int
undefinedNoLastError =
    fromParsedInput Undefined


undefinedWithLastError : Model Int
undefinedWithLastError =
    fromParsedInputAndLastError Undefined { input = "a", info = "parse error" }


testSuite : Test
testSuite =
    describe "Guarded.Input.Types tests"
        [ describe "inputStringMaybe tests"
            [ test "Valid model (with no last error) has input string" <|
                \() ->
                    Just "5"
                        |> Expect.equal (inputStringMaybe validModelNoLastError)
            , test "Valid model (with last error) also has input string" <|
                \() ->
                    Just "5"
                        |> Expect.equal (inputStringMaybe validModelWithLastError)
            , test "Work-in-progress state has the input string" <|
                \() ->
                    Just "-"
                        |> Expect.equal (inputStringMaybe workInProgress)
            , test "Undefined model (with no last error) has no input string" <|
                \() ->
                    Nothing
                        |> Expect.equal (inputStringMaybe undefinedNoLastError)
            , test "Undefined model (with last error) also has no input string" <|
                \() ->
                    Nothing
                        |> Expect.equal (inputStringMaybe undefinedWithLastError)
            ]
        , describe "Some inputString tests"
            [ test "Valid model (with no last error) has input string" <|
                \() ->
                    "5"
                        |> Expect.equal (inputString validModelNoLastError)
            , test "Undefined model (with no last error) has empty input string" <|
                \() ->
                    ""
                        |> Expect.equal (inputString undefinedNoLastError)
            ]
          -- TODO test lastError
          -- TODO test toResult
        ]
