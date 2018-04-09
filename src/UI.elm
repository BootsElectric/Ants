module UI exposing ( UI, writeUI, writePostGame, writePreGame )

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

writePreGame : Model -> Html Msg
writePreGame model =
    layout stylesheet <|
      screen <|
        Element.column StyleOne
          [ center
          , verticalCenter
          , width ( px <| toFloat <| model.dimensions.width )
          , height ( px <| toFloat <| model.dimensions.height )
          ]
          [ Element.header GameOver [] ( Element.text "Welcome To Formica" )
          , Element.text "A game about ant colonies"
          , Element.button Button
            [ width ( px 160 )
            , height ( px 40 )
            , Events.onClick <| UpdateState InGame
            ]
             ( Element.text "Start Game" )
          ]

writePostGame : Model -> Html Msg
writePostGame model =
    layout stylesheet <|
      screen <|
        Element.column StyleOne
          [ center
          , verticalCenter
          , width ( px <| toFloat <| model.dimensions.width )
          , height ( px <| toFloat <| model.dimensions.height )
          ]
          [ Element.header GameOver [] ( Element.text "Game Over" )
          , Element.button Button
            [ width ( px 160 )
            , height ( px 40 )
            , onClick <| UpdateState PreGame
            ]
            (Element.text "Back To Start")
          ]


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
                Element.button Button
                  [ width (px 160)
                  , Events.onClick Collect
                  ]
                  ( Element.text "Collect" )

          Nothing ->
              el StyleOne [] ( Element.text "" )
  in

    layout stylesheet <|
      screen <|
        el MainBackground [ width ( px 180 ), height ( px (toFloat model.dimensions.height) ) ]
          ( column MainBackground
           [ width ( px 180 )
           , paddingLeft 10
           , paddingTop 25
           , paddingBottom 25
           , alignLeft
           , spacing 25
           ]
            [ ( Element.text ( String.concat [ "Selected: (", selX, ",", selY, ")"  ] ) )
            , row StyleOne [ spacing 10 ]
              [ ( el StyleOne [ paddingTop 15 ] ( Element.text kind ) )
              , image StyleOne [ width ( px 64 ), height ( px 64 ) ] { src = src, caption = kind }
              ]
            , Element.text ( String.concat [ "Population: ", ( toString <| round model.ants.number ) ] )
            , Element.text ( String.concat [ "Food: ", ( toString <| round model.ants.food ) ] )
            , collectButton
            , Element.button Button
              [ width ( px 160 )
              , Events.onClick <| UpdateState PostGame
              ]
              ( Element.text "Quit" )
            ] )
