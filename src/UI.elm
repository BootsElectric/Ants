module UI exposing ( UI, writeUI )

import Html exposing (..)

import Element exposing (..)
import Element.Attributes as Attr exposing (..)
import Element.Events as Events exposing (..)
import StyleSheet exposing (..)

import Messages exposing ( Msg(..) )
import Model exposing (..)
import Textures exposing (..)
import Tile exposing (..)
import UnMaybe exposing ( unMaybeInt )

type UI =
  UI

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

    collectButton =
      case model.selected of
          Just tile ->
            case tile.kind of
              Dirt ->
                el StyleOne [] ( Element.text "" )
              Queen ->
                el StyleOne [] ( Element.text "" )
              Food ->
                Element.button Button [ width (px 160) , Events.onClick Collect ] ( Element.text "Collect" )

          Nothing ->
              el StyleOne [] ( Element.text "" )
  in

    layout stylesheet <|
      screen <|
        el StyleOne [ width ( px 180 ), height ( px (toFloat model.dimensions.height) ) ]
          ( column StyleOne [ width ( px 180 ), paddingLeft 10, paddingTop 25, paddingBottom 25, alignLeft, spacing 25 ]
            [ ( Element.text ( String.concat [ "Selected: (", selX, ",", selY, ")"  ] ) )
            , row StyleOne [ spacing 10 ]
              [ ( el StyleOne [ paddingTop 15 ] ( Element.text kind ) )
              , image StyleOne [ width ( px 64 ), height ( px 64 ) ] { src = src, caption = kind }
              ]
            , Element.text ( String.concat [ "Population: ", ( toString model.ants.number ) ] )
            , collectButton
            ] )
