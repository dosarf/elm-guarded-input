module Guarded.Input.CssUtilTests exposing (testSuite)

import Test exposing (..)
import Expect
import Fuzz exposing (int, string)
import Guarded.Input.CssUtil exposing (Purpose(..), classListForInput, classListForWarning)
import Guarded.Input.InternalTypes exposing (..)
import Guarded.Input exposing (..)


testClassPurposes : List ( String, Guarded.Input.CssUtil.Purpose )
testClassPurposes =
    [ ( "valid", Valid )
    , ( "invalid", Invalid )
    , ( "work-in-progress", WorkInProgress )
    , ( "undefined", Undefined )
    ]


validModel : Model Bool
validModel =
    Model_ { parsedInput = Valid_ ( True, "True" ), lastError = Nothing }


workInProgressModel : Model Bool
workInProgressModel =
    Model_ { parsedInput = (WorkInProgress_ "Tru"), lastError = Nothing }


workInProgressMsg : Msg Bool
workInProgressMsg =
    WorkInProgressMsg_ ( "Tru", "Tru could continue with e" )


undefinedModel : Model value
undefinedModel =
    Model_ { parsedInput = Undefined_, lastError = Nothing }


withLastError : Model value -> Model value
withLastError (Model_ model) =
    Model_ { model | lastError = Just (LastError_ "test" "test") }


testSuite : Test
testSuite =
    describe "Guarded.Util.CssUtil tests"
        [ describe "classListForInput tests"
            [ test "valid model yields the corresponding class" <|
                \() ->
                    [ "valid" ]
                        |> Expect.equal (classListForInput testClassPurposes validModel |> findSelectedClasses)
            , test "Work-in-progress model yields the corresponding class" <|
                \() ->
                    [ "work-in-progress" ]
                        |> Expect.equal (classListForInput testClassPurposes workInProgressModel |> findSelectedClasses)
            , test "Undefined model yields the corresponding class" <|
                \() ->
                    [ "undefined" ]
                        |> Expect.equal (classListForInput testClassPurposes undefinedModel |> findSelectedClasses)
            ]
        , describe "classListForWarning tests"
            [ test "valid model yields the corresponding class" <|
                \() ->
                    [ "valid" ]
                        |> Expect.equal (classListForWarning testClassPurposes validModel |> findSelectedClasses)
            , test "Work-in-progress model yields the corresponding class" <|
                \() ->
                    [ "work-in-progress" ]
                        |> Expect.equal (classListForWarning testClassPurposes workInProgressModel |> findSelectedClasses)
            , test "Undefined model yields the corresponding class" <|
                \() ->
                    [ "undefined" ]
                        |> Expect.equal (classListForWarning testClassPurposes undefinedModel |> findSelectedClasses)
            , test "valid model with last error yields the corresponding class" <|
                \() ->
                    [ "invalid" ]
                        |> Expect.equal (withLastError validModel |> classListForWarning testClassPurposes |> findSelectedClasses)
            , test "Work-in-progress model with last error yields the corresponding class" <|
                \() ->
                    [ "invalid" ]
                        |> Expect.equal (withLastError workInProgressModel |> classListForWarning testClassPurposes |> findSelectedClasses)
            , test "Undefined model with last error yields the corresponding class" <|
                \() ->
                    [ "invalid" ]
                        |> Expect.equal (withLastError undefinedModel |> classListForWarning testClassPurposes |> findSelectedClasses)
            ]
        ]


findSelectedClasses : List ( String, Bool ) -> List String
findSelectedClasses classAndBools =
    List.filter (\( class, selected ) -> selected) classAndBools |> List.map (\( class, selected ) -> class)
