module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Guarded.Util.IntegerTests exposing (testSuite)
import Guarded.Input.ParsersTests exposing (testSuite)
import Guarded.Input.CssUtilTests exposing (testSuite)
import Guarded.InputTests exposing (testSuite)


all : Test
all =
    describe "All tests for Elm package Guarded.Input"
        [ Guarded.Util.IntegerTests.testSuite
        , Guarded.Input.ParsersTests.testSuite
        , Guarded.Input.CssUtilTests.testSuite
        , Guarded.InputTests.testSuite
        ]
