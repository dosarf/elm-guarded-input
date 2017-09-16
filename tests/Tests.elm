module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Guarded.Util.IntegerTests exposing (testSuite)
import Guarded.Input.TypesTests exposing (testSuite)
import Guarded.Input.StateTests exposing (testSuite)
import Guarded.Input.ParserTests exposing (testSuite)


all : Test
all =
    describe "Guarded.Input tests"
        [ Guarded.Util.IntegerTests.testSuite
        , Guarded.Input.TypesTests.testSuite
        , Guarded.Input.StateTests.testSuite
        , Guarded.Input.ParserTests.testSuite
        ]
