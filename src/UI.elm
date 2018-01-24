module UI exposing ( UI, writeUI )

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Messages exposing ( Msg(..) )
import Model exposing (..)
import Tile exposing (..)

type UI =
  UI

writeUI : Model -> Html Msg
writeUI model =
  let
    selX =
      toString <| getX model.selected

    selY =
      toString <| getY model.selected
  in

    div
    [Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "25%" ) ]]
    [ p [] [ Html.text <| "Selected: (" ++  selX ++ ", " ++ selY ++ ")" ] ]
