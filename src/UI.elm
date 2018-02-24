module UI exposing ( UI, writeUI, unMaybeInt )

import Html exposing (..)

import Element exposing (..)
import Element.Attributes as Attr exposing (..)
--import Style exposing (..)
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

    layout stylesheet <|
      screen <| el StyleOne
        [ moveDown 875, moveLeft 750, width ( px 200 ), paddingTop 25, paddingBottom 25, center ]
                ( Element.text ( String.concat [ "Selected: (", selX, ",", selY, ")"  ] ) )

    --div
    --[Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "25%" ) ]]
    --[ p [] [ Html.text <| "Selected: (" ++  selX ++ ", " ++ selY ++ ")" ] ]
