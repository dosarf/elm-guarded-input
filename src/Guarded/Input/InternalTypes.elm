module Guarded.Input.InternalTypes exposing (Info_, Input_, LastError_, Model_(..), Msg_(..), ParsedInput_(..), fromParsedInput, fromParsedInputAndLastError)


type alias Input_ =
    String


type alias Info_ =
    String


type ParsedInput_ value
    = Valid_ ( value, Input_ )
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
    = ValidMsg_ ( value, Input_ )
    | InvalidMsg_ ( Input_, Info_ )
    | WorkInProgressMsg_ ( Input_, Info_ )
    | UndefinedMsg_


fromParsedInput : ParsedInput_ value -> Model_ value
fromParsedInput parsedInput =
    Model_ { parsedInput = parsedInput, lastError = Nothing }


fromParsedInputAndLastError : ParsedInput_ value -> LastError_ -> Model_ value
fromParsedInputAndLastError parsedInput lastError =
    Model_ { parsedInput = parsedInput, lastError = Just lastError }
