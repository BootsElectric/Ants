module UI exposing ( UI, writeUI, unMaybeInt )

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Element exposing (..)
import Style
import StyleSheet exposing (..)

import Messages exposing ( Msg(..) )
import Model exposing (..)
import Tile exposing (..)

type UI =
  UI

unMaybeInt : Maybe Int -> Int
unMaybeInt int =
  case int of
      Just int ->
           int

      Nothing ->
          0


writeUI : Model -> Html Msg
writeUI model =
  let
    selX =
      toString <| unMaybeInt <| getX model.selected

    selY =
      toString <| unMaybeInt <| getY model.selected
  in

    div
    [Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "25%" ) ]]
    [ p [] [ Html.text <| "Selected: (" ++  selX ++ ", " ++ selY ++ ")" ] ]
