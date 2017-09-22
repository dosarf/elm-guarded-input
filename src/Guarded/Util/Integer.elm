module Guarded.Util.Integer exposing (..)

{- Apparently you can get an Int that is NaN (probably a bug, e.g. String.toInt "-").
   And you can't easily test for it, because isNaN is for Floats only.
-}


intIsNaN : Int -> Bool
intIsNaN n =
    toFloat n |> isNaN


nonNaNIntResult : Int -> Result String Int
nonNaNIntResult n =
    if intIsNaN n then
        Err "NaN"
    else
        Ok n
