# Guarded input controls

Guarded here means that the input (text) control is simply not allowed to
contain any erroneous string. This is solved by constant normalization of
the input control's content model and by feeding back the normalized model
to the view of the input control.

Potentially handy in educational software, where one does not want to confuse kids
with explanations of badly formed input and such like.

A guarded input can be in one of three _acceptable_ states:
- undefined (empty input control),
- work-in-progress (not convertible to a useful value, but could evolve
potentially into one), and
- valid (has actual value).

If the `Guarded.Input.update` function rejects a change of the input contents,
the previously accepted state is preserved, and that model should be written back
to the view - this way erroneous attempts will be just discarded.

## What's with work-in-progress?
Erroneous content of input control is disallowed (actually, discarded immediately,
as explained above). But there are contents that, while themselves are not yet
convertible to a valid value, can evolve into such. For instance, a single
minus character ("-") can evolve into a valid negative number. Such content
must be allowed in the input control. The model in this case does not have
a valid value to use (how could it?), but the work-in-progress string is
stored nevertheless in the model, to be able to feed it back to the input
control when generating the view. See `Guarded.Input.inputString`.

## Example for a guarded input control accepting integers
Note that this library decidedly deals with input controls of text type. No
spinners in the case of this example, for instance.

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

Check this [demo](https://github.com/dosarf/elm-guarded-input/tree/master/src/Demo).
