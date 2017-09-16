module Guarded.Input.StateTests exposing (..)

import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input.Types exposing (..)
import Guarded.Input.State exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (int)
import String


validModel : Model Bool
validModel =
    Model { parsedInput = Valid True, lastError = Nothing }


validMsg : Msg Bool
validMsg =
    Valid_ True


workInProgressModel : Model Bool
workInProgressModel =
    Model { parsedInput = (WorkInProgress "Tru"), lastError = Nothing }


workInProgressMsg : Msg Bool
workInProgressMsg =
    WorkInProgress_ ( "Tru", "Tru could continue with e" )


undefinedModel : Model value
undefinedModel =
    Model { parsedInput = Undefined, lastError = Nothing }


undefinedMsg : Msg value
undefinedMsg =
    Undefined_


undefinedModelWithError : Model value
undefinedModelWithError =
    Model { parsedInput = Undefined, lastError = Just <| LastError "Tre" "Tre is in no way valid boolean value" }


invalidMsg : Msg Bool
invalidMsg =
    Invalid_ ( "Tre", "Tre is in no way valid boolean value" )


workInProgressModelWithError : Model Bool
workInProgressModelWithError =
    Model { parsedInput = WorkInProgress "Tru", lastError = Just <| LastError "Tre" "Tre is in no way valid boolean value" }


validModel2 : Model Bool
validModel2 =
    Model { parsedInput = Valid False, lastError = Nothing }


validMsg2 : Msg Bool
validMsg2 =
    Valid_ False


validModelWithError : Model Bool
validModelWithError =
    Model { parsedInput = Valid True, lastError = Just <| LastError "Tre" "Tre is in no way valid boolean value" }


testSuite : Test
testSuite =
    describe "Guarded.Input.State tests"
        [ describe "init tests"
            [ test "Initial model has no values, the working input empty, and the message is 'Undefined'" <|
                \() ->
                    undefinedModel
                        |> Expect.equal init
            ]
        , describe "initFor tests"
            [ fuzz int "Model initialized for a given value has that value, and there is no last error" <|
                \x ->
                    Model { parsedInput = Valid x, lastError = Nothing }
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
