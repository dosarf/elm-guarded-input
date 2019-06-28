module Tests exposing (all)

import Expect
import Fuzz exposing (int, list, string, tuple)
import Guarded.Input.CssUtilTests exposing (testSuite)
import Guarded.Input.ParsersTests exposing (testSuite)
import Guarded.InputTests exposing (testSuite)
import String
import Test exposing (..)


all : Test
all =
    describe "All tests for Elm package Guarded.Input"
        [ Guarded.Input.ParsersTests.testSuite
        , Guarded.Input.CssUtilTests.testSuite
        , Guarded.InputTests.testSuite
        ]
