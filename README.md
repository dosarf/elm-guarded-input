# elm-guarded-input
Simple input field guarding (normalization)

```
type alias Model =
    { parsedInteger : Guarded.Input.Model Int
    , ...
    }

type Msg
    = IntegerChanged (Guarded.Input.Msg Int)
    | ...

view : Model -> Html Msg
view model =
    ...
    input
        [ Guarded.Input.parseOnInput IntegerChanged Guarded.Input.Parsers.intParser
        , value <| Guarded.Input.inputString model.parsedInteger
        ]
        []
    ...
```
