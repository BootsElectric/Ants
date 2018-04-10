module UnMaybe exposing (..)

{-|
  Remove the Maybe from a Maybe Int or return 0 on Nothing
    int - the Int to remove Maybe from
-}
unMaybeInt : Maybe Int -> Int
unMaybeInt int =
  case int of
      Just int ->
           int

      Nothing ->
          0

{-|
  Remove the Maybe from a Maybe Float or return 0 on Nothing
    float - the Float to remove Maybe from
-}
unMaybeFloat : Maybe Float -> Float
unMaybeFloat float =
  case float of
      Just float ->
           float

      Nothing ->
          0
