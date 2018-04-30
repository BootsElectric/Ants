module UI exposing ( writeUI, writePostGame, writePreGame )

import Html exposing (..)

import Element exposing (..)
import Element.Attributes as Attr exposing (..)
import Element.Events as Events exposing (..)

import StyleSheet exposing (..)
import Messages exposing ( Msg(..) )
import Model exposing (..)
import Textures exposing (..)
import Tile exposing (..)
import UnMaybe exposing ( unMaybeInt, unMaybeFloat )

{-|
  Writes the UI of the PreGame State
    model - the Model to generate the UI from
-}
writePreGame : Model -> Html Msg
writePreGame model =
    layout stylesheet <|
      screen <|
        Element.column StyleOne
          [ center
          , verticalCenter
          , width ( px <| toFloat <| model.dimensions.width )
          , height ( px <| toFloat <| model.dimensions.height )
          , spacing 50
          ]
          [ Element.header GameOver [] ( Element.text "Welcome To Formica" )
          , Element.text "A game about ant colonies"
          , Element.text "How to play:"
          , Element.text "Click on a tile to interact"
          , Element.text "Use W, A, S and D to move the camera"
          , Element.text "Red arrows indicate the nearest Disaster"
          , Element.text "Green arrows indicate the nearest Food"
          , Element.text "Dig through all the tiles to win!"
          , Element.button Button
            [ width ( px 160 )
            , height ( px 40 )
            , Events.onClick <| Generate
            ]
             ( Element.text "Start Game" )
          ]

{-|
  Writes the UI of the PostGame State
    model - the Model to generate the UI from
-}
writePostGame : Model -> Html Msg
writePostGame model =
  let
    winText =
      if model.won then
          Element.text "You Won!"
      else
          Element.text "Game Over"

    score =
      624 - model.undugTiles
  in
    layout stylesheet <|
      screen <|
        Element.column StyleOne
          [ center
          , verticalCenter
          , width ( px <| toFloat <| model.dimensions.width )
          , height ( px <| toFloat <| model.dimensions.height )
          , spacing 50
          ]
          [ Element.header GameOver [] ( winText )
          , Element.text ( String.concat [ "Your score: ", ( toString score ) ] )
          , Element.button Button
            [ width ( px 160 )
            , height ( px 40 )
            , onClick <| UpdateState PreGame
            ]
            (Element.text "Back To Start")
          ]

{-|
  Writes the UI for the InGame State
    model - the Model to generate the UI from
-}
writeUI : Model -> Html Msg
writeUI model =
  let
    selX =
      toString <| unMaybeInt <| getX model.selected

    selY =
      toString <| unMaybeInt <| getY model.selected

    float =
      toString <| unMaybeFloat <| getFloat model.selected

    kind =
      Tile.getKindAsString model.selected

    src =
      case model.selected of
        Just tile ->
          if tile.isDug == False then
            String.concat [ texturesUrl, undugUrl ]

          else

            case tile.kind of
              Dirt ->
                String.concat [ texturesUrl, dirtUrl ]

              Queen ->
                String.concat [ texturesUrl, queenUrl ]

              Food ->
                String.concat [ texturesUrl, foodUrl ]

              Disaster ->
                String.concat [ texturesUrl, disasterUrl ]

        Nothing ->
          "Nothing Selected"

    collectButton =
      case model.selected of
          Just tile ->
            if tile.isDug == False then
                Element.button Button [ width (px 160)
                , onClick Dig
                ]
                ( Element.text "Dig" )

            else

              case tile.kind of

                Food ->
                  Element.button Button
                    [ width (px 160)
                    , Events.onClick Collect
                    ]
                    ( Element.text "Collect" )

                _ ->
                  el StyleOne [] ( Element.text "" )

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
            , Element.text ( String.concat [ "Population: ", ( toString <| model.ants.number ) ] )
            , Element.text ( String.concat [ "Food: ", ( toString <| model.ants.food ) ] )
            , Element.text ( String.concat [ "Score: ", ( toString <| 625 - model.undugTiles ) ] )
            , collectButton
            , Element.button Button
              [ width ( px 160 )
              , Events.onClick <| UpdateState PostGame
              ]
              ( Element.text "Quit" )
            ] )
