module Guarded.InputTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (int)
import String
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input exposing (..)


testSuite : Test
testSuite =
    describe "Guarded.Input tests"
        [ modelStateTestSuite
        , utilityFunctionsTest
        ]



-- Model state tests


validModel : Model Bool
validModel =
    Model_ { parsedInput = Valid_ ( True, "True" ), lastError = Nothing }


validMsg : Msg Bool
validMsg =
    ValidMsg_ ( True, "True" )


workInProgressModel : Model Bool
workInProgressModel =
    Model_ { parsedInput = (WorkInProgress_ "Tru"), lastError = Nothing }


workInProgressMsg : Msg Bool
workInProgressMsg =
    WorkInProgressMsg_ ( "Tru", "Tru could continue with e" )


undefinedModel : Model value
undefinedModel =
    Model_ { parsedInput = Undefined_, lastError = Nothing }


undefinedMsg : Msg value
undefinedMsg =
    UndefinedMsg_


undefinedModelWithError : Model value
undefinedModelWithError =
    Model_ { parsedInput = Undefined_, lastError = Just <| LastError_ "Tre" "Tre is in no way valid boolean value" }


invalidMsg : Msg Bool
invalidMsg =
    InvalidMsg_ ( "Tre", "Tre is in no way valid boolean value" )


workInProgressModelWithError : Model Bool
workInProgressModelWithError =
    Model_ { parsedInput = WorkInProgress_ "Tru", lastError = Just <| LastError_ "Tre" "Tre is in no way valid boolean value" }


validModel2 : Model Bool
validModel2 =
    Model_ { parsedInput = Valid_ ( False, "False" ), lastError = Nothing }


validMsg2 : Msg Bool
validMsg2 =
    ValidMsg_ ( False, "False" )


validModelWithError : Model Bool
validModelWithError =
    Model_ { parsedInput = Valid_ ( True, "True" ), lastError = Just <| LastError_ "Tre" "Tre is in no way valid boolean value" }


modelStateTestSuite : Test
modelStateTestSuite =
    describe "Guarded.Input model state tests"
        [ describe "init tests"
            [ test "Initial model has no values, the working input empty, and the message is 'Undefined'" <|
                \() ->
                    undefinedModel
                        |> Expect.equal init
            ]
        , describe "initFor tests"
            [ fuzz int "Model initialized for a given value has that value, and there is no last error" <|
                \x ->
                    Model_ { parsedInput = Valid_ ( x, toString x ), lastError = Nothing }
                        |> Expect.equal (initFor x)
            ]
        , describe "update tests for undefined models"
            [ test "Undefined model is turned valid by valid msg" <|
                \() ->
                    validModel
                        |> Expect.equal (Tuple.first (update validMsg undefinedModel))
            , test "Undefined model is turned work-in-progress (no last error) with accepted input by work-in-progress msg" <|
                \() ->
                    workInProgressModel
                        |> Expect.equal (Tuple.first (update workInProgressMsg undefinedModel))
            , test "Undefined model is left undefined by an undefined msg" <|
                \() ->
                    undefinedModel
                        |> Expect.equal (Tuple.first (update undefinedMsg undefinedModel))
            , test "Undefined model is left undefined by an invalid msg + some error info" <|
                \() ->
                    undefinedModelWithError
                        |> Expect.equal (Tuple.first (update invalidMsg undefinedModel))
            ]
        , describe "update tests for work-in-progress models"
            [ test "work-in-progress model is made undefined by undefined message" <|
                \() ->
                    undefinedModel
                        |> Expect.equal (Tuple.first (update undefinedMsg workInProgressModel))
            , test "work-in-progress model is still work-in-progress (no errors) by work-in-progress message" <|
                \() ->
                    workInProgressModel
                        |> Expect.equal (Tuple.first (update workInProgressMsg workInProgressModel))
            , test "work-in-progress model is made valid by valid message" <|
                \() ->
                    validModel
                        |> Expect.equal (Tuple.first (update validMsg workInProgressModel))
            , test "work-in-progress model is made work-in-progress (+ error) by invalid message" <|
                \() ->
                    workInProgressModelWithError
                        |> Expect.equal (Tuple.first (update invalidMsg workInProgressModel))
            ]
        , describe "update tests for valid models"
            [ test "valid model is made undefined by undefined message" <|
                \() ->
                    undefinedModel
                        |> Expect.equal (Tuple.first (update undefinedMsg validModel))
            , test "valid model is made work-in-progress (no error) by work-in-progress message" <|
                \() ->
                    workInProgressModel
                        |> Expect.equal (Tuple.first (update workInProgressMsg validModel))
            , test "valid model is made valid by valid message" <|
                \() ->
                    validModel2
                        |> Expect.equal (Tuple.first (update validMsg2 validModel))
            , test "valid model is still valid (+ error message) after getting invalid msg" <|
                \() ->
                    validModelWithError
                        |> Expect.equal (Tuple.first (update invalidMsg validModel))
            ]
        ]



-- Utility functions tests


validModelNoLastError : Model Int
validModelNoLastError =
    fromParsedInput <| Valid_ ( 5, "5" )


validModelWithLastError : Model Int
validModelWithLastError =
    fromParsedInputAndLastError (Valid_ ( 5, "5" )) { input = "5a", info = "parse error" }


workInProgress : Model Int
workInProgress =
    fromParsedInput <| WorkInProgress_ "-"


undefinedNoLastError : Model Int
undefinedNoLastError =
    fromParsedInput Undefined_


undefinedWithLastError : Model Int
undefinedWithLastError =
    fromParsedInputAndLastError Undefined_ { input = "a", info = "parse error" }


utilityFunctionsTest : Test
utilityFunctionsTest =
    describe "Guarded.Input utility functions tests"
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
