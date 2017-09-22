module Guarded.Input.InternalTypes exposing (..)


type alias Input_ =
    String


type alias Info_ =
    String


type ParsedInput_ value
    = Valid_ value
    | WorkInProgress_ Input_
    | Undefined_


type alias LastError_ =
    { input : Input_
    , info : Info_
    }


type Model_ value
    = Model_
        { parsedInput : ParsedInput_ value
        , lastError : Maybe LastError_
        }


type Msg_ value
    = ValidMsg_ value
    | InvalidMsg_ ( Input_, Info_ )
    | WorkInProgressMsg_ ( Input_, Info_ )
    | UndefinedMsg_
