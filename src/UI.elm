module UI exposing ( UI, writeUI, unMaybeInt )

import Html exposing (..)

import Element exposing (..)
import Element.Attributes as Attr exposing (..)
--import Style exposing (..)
import StyleSheet exposing (..)

import Messages exposing ( Msg(..) )
import Model exposing (..)
import Textures exposing (..)
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

    kind =
      Tile.getKind model.selected

    src =
      case model.selected of
        Just tile ->
          case tile.kind of
            Dirt ->
              String.concat [ texturesUrl, dirtUrl ]

            Queen ->
              String.concat [ texturesUrl, queenUrl ]

            Food ->
              String.concat [ texturesUrl, foodUrl ]

        Nothing ->
          "Nothing Selected"
  in

    layout stylesheet <|
      screen <|
        column StyleOne [ width ( px 200 ), paddingLeft 10, paddingTop 25, paddingBottom 25, alignLeft, spacing 25 ]
                [ ( Element.text ( String.concat [ "Selected: (", selX, ",", selY, ")"  ] ) )
                , row StyleOne [ spacing 10 ]
                  [ ( el StyleOne [ paddingTop 15 ] ( Element.text kind ) )
                  , image StyleOne [ width ( px 64 ), height ( px 64 ) ] { src = src, caption = kind }
                  ]
                , Element.text "Population: 1000"
                ]



    --div
    --[Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "25%" ) ]]
    --[ p [] [ Html.text <| "Selected: (" ++  selX ++ ", " ++ selY ++ ")" ] ]
