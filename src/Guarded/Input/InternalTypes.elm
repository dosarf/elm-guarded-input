module Guarded.Input.InternalTypes exposing (..)


type alias Input =
    String


type alias Info =
    String


type ParsedInput value
    = Valid value
    | WorkInProgress Input
    | Undefined


type alias LastError =
    { input : Input
    , info : Info
    }
