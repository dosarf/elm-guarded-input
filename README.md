# Guarded input controls

Guarded here means that the input control is simply not allowed to
contain any erroneous string. This is solved by normalization of
the input control's content model on every input event and by feeding back
the normalized model to the view of the input control.

Potentially handy in educational software, where one does not want to confuse kids
with explanations of badly formed input and such like.

A guarded input can be in one of three _acceptable_ states:
- undefined (empty input control),
- work-in-progress (not convertible to a useful value, but could evolve
potentially into one), and
- valid (has actual value).

If the `Guarded.Input.update` function rejects a change of the input contents,
the previous state - necessarily an _accepted_ one - is preserved, and that
is the model to write back to the view, and this way erroneous
attempts will be just discarded.

## Example for a guarded input control accepting integers
Note that this library decidedly deals with input controls of text type. No
spinners, for instance.

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
        , classList <| classListForInput myClasses model.parsedInteger
        ]
        []
    ...

myClasses : List ( String, Guarded.Input.CssUtil.Purpose )
myClasses =
    [ ( "guarded-input-valid", Guarded.Input.CssUtil.Valid )
    , ( "guarded-input-work-in-progress", Guarded.Input.CssUtil.WorkInProgress )
    , ( "guarded-input-undefined", Guarded.Input.CssUtil.Undefined )
    ]
```

Check the source for a
[demo appliciation](https://github.com/dosarf/elm-guarded-input/tree/master/src/Demo).
