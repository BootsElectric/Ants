module UnMaybe exposing (..)

unMaybeInt : Maybe Int -> Int
unMaybeInt int =
  case int of
      Just int ->
           int

      Nothing ->
          0


unMaybeFloat : Maybe Float -> Float
unMaybeFloat float =
  case float of
      Just float ->
           float

      Nothing ->
          0
