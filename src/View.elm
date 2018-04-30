module View exposing ( view, isWithin, findIndicators )

import Html exposing ( Html, div )
import Html.Attributes as Attr exposing ( style )
import Tuple exposing ( first, second )
import Dict exposing (..)
--
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.TwoD.Render as Render exposing ( Renderable, rectangle )
import Game.Resources as Resources exposing (Resources)
import ElementRelativeMouseEvents as Canvas exposing ( onMouseMove )
--
import Messages exposing ( Msg(..) )
import Model exposing ( Model, Column, Coord, State(..) )
import Textures exposing (..)
import Tile exposing ( Tile, Kind(..), getX, getY, Grid )
import UI exposing (..)
import UnMaybe exposing (..)

{-|
  renders the grid of tiles
-}
renderGrid : Model -> List Renderable
renderGrid model =
  List.map (renderCoord model model.camera model.resources) ( values <| model.grid )


{-|
  Renders a single tile
-}
renderCoord : Model -> Camera -> Resources -> Tile -> Renderable
renderCoord model camera resources tile =
  let
    x = unMaybeInt <| getX <| Just tile

    y = unMaybeInt <| getY <| Just tile

    position =
      ( toFloat x, toFloat y )

    texture =
      if tile.isDug == False then
          useUrl model camera x y undugUrl
      else

        case tile.kind of
          Dirt ->
            chooseIndicatorUrl model camera x y tile

          Queen ->
            useUrl model camera x y queenUrl

          Food ->
            useUrl model camera x y foodUrl

          Disaster ->
            useUrl model camera x y disasterUrl
  in
    Render.sprite
      { texture = Resources.getTexture texture resources
      , position = position
      , size = ( 1, 1 )
      }


{-|
  Finds the correct URL to use to load the correct texture,
  either the highlighted texture, the selected texture or the normal texture
    model - the model to use in helper functions
    camera - the camera to use in helper functions
    x - the x coordinate of the tile being rendered
    y - the y coordinate of the tile being rendered
    kind - string reperesentation of the tile being rendered's tile.kind
-}
useUrl : Model -> Camera -> Int -> Int -> String -> String
useUrl model camera x y kind =
    if isWithin model camera x y then

      String.concat [texturesUrl, "h-", kind]

    else if isSelected model x y then

      String.concat [texturesUrl, "s-", kind]

    else

      String.concat [texturesUrl, kind]

{-}
chooseIndicatorUrl : Model -> Camera -> Int -> Int -> Tile -> String
chooseIndicatorUrl model camera x y tile =
    let
      prefix =
        if isWithin model camera x y then

          "h-"

        else if isSelected model x y then

          "s-"

        else

          ""

      indicators =

        findIndicators tile model.grid

    in
      if indicators == "" then
          String.concat [ texturesUrl, prefix, dirtUrl ]
      else

          String.concat [ texturesUrl, prefix, indicators, indicatorUrl ]
-}

chooseIndicatorUrl : Model -> Camera -> Int -> Int -> Tile -> String
chooseIndicatorUrl model camera x y tile =
  let
    prefix =
      if isWithin model camera x y then

        "h-"

      else if isSelected model x y then

        "s-"

      else

        ""

    indicators =

      case tile.indicator of
        Tile.DB ->
          "db-"
        Tile.DL ->
          "dl-"
        Tile.DR ->
          "dr-"
        Tile.DT ->
          "dt-"
        Tile.FB ->
          "fb-"
        Tile.FL ->
          "fl-"
        Tile.FR ->
          "fr-"
        Tile.FT ->
          "ft-"
        Tile.BB ->
          "db-fb-"
        Tile.BL ->
          "dl-fl-"
        Tile.BR ->
          "dr-fr-"
        Tile.BT ->
          "dt-ft-"
        Tile.DBFL ->
          "db-fl-"
        Tile.DBFR ->
          "db-fr-"
        Tile.DBFT ->
          "db-ft-"
        Tile.DLFB ->
          "dl-fb-"
        Tile.DLFR ->
          "dl-fr-"
        Tile.DLFT ->
          "dl-ft-"
        Tile.DRFB ->
          "dr-fb-"
        Tile.DRFL ->
          "dr-fl-"
        Tile.DRFT ->
          "dr-ft-"
        Tile.DTFB ->
          "dt-fb-"
        Tile.DTFL ->
          "dt-fl-"
        Tile.DTFR ->
          "dt-fr-"
        Tile.None ->
          ""
  in
    String.concat [ texturesUrl, prefix, indicators, indicatorUrl ]

findIndicators : Tile -> Grid -> String
findIndicators tile grid =
    let
        closestDisaster =
          findClosestDisaster tile grid

        closestFood =
          findClosestFood tile grid

    in

        String.concat [ closestDisaster, closestFood ]


findClosestDisaster : Tile -> Grid -> String
findClosestDisaster tile grid =
    let
        closestHorizontal =
          findClosestHorizontalDisaster 1 tile grid

        closestVertical =
          findClosestVerticalDisaster 1 tile grid
    in
        if closestHorizontal == 0 && closestVertical == 0 then
            ""
        else
          if closestHorizontal == 0 then
              if closestVertical > 0 then
                  "dt-"
              else
                  "db-"
          else
            if closestVertical == 0 then
              if closestHorizontal > 0 then
                  "dr-"
              else
                  "dl-"
            else
              if abs closestHorizontal <= abs closestVertical then
                  if closestHorizontal > 0 then
                      "dr-"
                  else
                      "dl-"
              else
                  if closestVertical > 0 then
                      "dt-"
                  else
                      "db-"


findClosestHorizontalDisaster : Int -> Tile -> Grid -> Int
findClosestHorizontalDisaster var tile grid =
    let
        xCoord =
          Tuple.first tile.coord

        yCoord =
          Tuple.second tile.coord

        nextPosotiveTile =
          Dict.get ( ( xCoord + var ), yCoord ) grid

        nextNegativeTile =
          Dict.get ( ( xCoord - var ), yCoord ) grid

        posotiveVar =
          case nextPosotiveTile of
            Just newTile ->
              if newTile.kind == Disaster then
                var
              else
                findClosestHorizontalDisaster ( var + 1 ) tile grid
            Nothing ->
              0

        negativeVar =
          case nextNegativeTile of
            Just newTile ->
              if newTile.kind == Disaster then
                -var
              else
                findClosestHorizontalDisaster ( var + 1 ) tile grid
            Nothing ->
              0
    in
      if posotiveVar == 0 && negativeVar /= 0 then
          negativeVar
      else if posotiveVar /= 0 && negativeVar == 0 then
          posotiveVar
      else if abs posotiveVar <= abs negativeVar then
          posotiveVar
      else
          negativeVar


findClosestVerticalDisaster : Int -> Tile -> Grid -> Int
findClosestVerticalDisaster var tile grid =
    let
        xCoord =
          Tuple.first tile.coord

        yCoord =
          Tuple.second tile.coord

        nextPosotiveTile =
          Dict.get ( xCoord, ( yCoord + var ) ) grid

        nextNegativeTile =
          Dict.get ( xCoord, ( yCoord - var ) ) grid

        posotiveVar =
          case nextPosotiveTile of
            Just newTile ->
              if newTile.kind == Disaster then
                var
              else
                findClosestVerticalDisaster ( var + 1 ) tile grid
            Nothing ->
              0

        negativeVar =
          case nextNegativeTile of
            Just newTile ->
              if newTile.kind == Disaster then
                -var
              else
                findClosestVerticalDisaster ( var + 1 ) tile grid
            Nothing ->
              0
    in
      if posotiveVar == 0 && negativeVar /= 0 then
          negativeVar
      else if posotiveVar /= 0 && negativeVar == 0 then
          posotiveVar
      else if abs posotiveVar <= abs negativeVar then
          posotiveVar
      else
          negativeVar

findClosestFood : Tile -> Grid -> String
findClosestFood tile grid =
    let
        closestHorizontal =
          findClosestHorizontalFood 1 tile grid

        closestVertical =
          findClosestVerticalFood 1 tile grid
    in
        if closestHorizontal == 0 && closestVertical == 0 then
            ""
        else
          if closestHorizontal == 0 then
              if closestVertical > 0 then
                  "ft-"
              else
                  "fb-"
          else
            if closestVertical == 0 then
              if closestHorizontal > 0 then
                  "fr-"
              else
                  "fl-"
            else
              if abs closestHorizontal <= abs closestVertical then
                  if closestHorizontal > 0 then
                      "fr-"
                  else
                      "fl-"
              else
                  if closestVertical > 0 then
                      "ft-"
                  else
                      "fb-"



findClosestHorizontalFood : Int -> Tile -> Grid -> Int
findClosestHorizontalFood var tile grid =
    let
        xCoord =
          Tuple.first tile.coord

        yCoord =
          Tuple.second tile.coord

        nextPosotiveTile =
          Dict.get ( ( xCoord + var ), yCoord ) grid

        nextNegativeTile =
          Dict.get ( ( xCoord - var ), yCoord ) grid

        posotiveVar =
          case nextPosotiveTile of
            Just newTile ->
              if newTile.kind == Food then
                var
              else
                findClosestHorizontalFood ( var + 1 ) tile grid
            Nothing ->
              0

        negativeVar =
          case nextNegativeTile of
            Just newTile ->
              if newTile.kind == Food then
                -var
              else
                findClosestHorizontalFood ( var + 1 ) tile grid
            Nothing ->
              0
    in
      if posotiveVar == 0 && negativeVar /= 0 then
          negativeVar
      else if posotiveVar /= 0 && negativeVar == 0 then
          posotiveVar
      else if abs posotiveVar <= abs negativeVar then
          posotiveVar
      else
          negativeVar


findClosestVerticalFood : Int -> Tile -> Grid -> Int
findClosestVerticalFood var tile grid =
    let
        xCoord =
          Tuple.first tile.coord

        yCoord =
          Tuple.second tile.coord

        nextPosotiveTile =
          Dict.get ( xCoord, ( yCoord + var ) ) grid

        nextNegativeTile =
          Dict.get ( xCoord, ( yCoord - var ) ) grid

        posotiveVar =
          case nextPosotiveTile of
            Just newTile ->
              if newTile.kind == Food then
                var
              else
                findClosestVerticalFood ( var + 1 ) tile grid
            Nothing ->
              0

        negativeVar =
          case nextNegativeTile of
            Just newTile ->
              if newTile.kind == Food then
                -var
              else
                findClosestVerticalFood ( var + 1 ) tile grid
            Nothing ->
              0
    in
      if posotiveVar == 0 && negativeVar /= 0 then
          negativeVar
      else if posotiveVar /= 0 && negativeVar == 0 then
          posotiveVar
      else if abs posotiveVar <= abs negativeVar then
          posotiveVar
      else
          negativeVar
{-|
  A function to test whether the tile being rendered is the selected tile
    m - the Model to get the grid and selected from
    x - the x coordinate of the tile being rendered
    y - the y coordinate of the tile being rendered
-}
isSelected : Model -> Int -> Int -> Bool
isSelected m x y =

  if Tile.getTile m.grid x y == m.selected then
    True
  else
    False



{-|
  Uses Camera.viewportToGameCoordinates to normalise mouse position then compares
  that position to the position of the tiles.

    m is the model
    camera is the camera
    x is the x coordinate of the tile in question
    y is the y coordinate of the tile in question

  Used in renderCoord to determine whether the coordinate is highlighted.
-}
isWithin : Model -> Camera -> Int -> Int -> Bool
isWithin m camera x y =
  let
      mousePos =
        Camera.viewportToGameCoordinates
           camera ( m.dimensions.width, m.dimensions.height ) ( round m.mPosX, round m.mPosY )

      mouseX =
        first mousePos

      mouseY =
        second mousePos

      withinX =
        ( ( mouseX >= ( toFloat x ) ) && ( mouseX <= ( ( toFloat x ) ) + 1 ) )

      withinY =
        ( ( mouseY >= ( toFloat y ) ) && ( mouseY <= ( ( toFloat y ) ) + 1 ) )
  in
    if withinX && withinY then
      True
    else
      False

{-|
  Generates the view to be used in Main's main function
    model - the Model to generate the view from
-}
view : Model -> Html Msg
view model =
  let
    ui =
      case model.state of
        PreGame ->
          writePreGame model

        InGame ->
          writeUI model

        PostGame ->
          writePostGame model

    tile =
      case model.selected of
          Just tile ->
              tile

          Nothing ->
              Tile.newTile ( 0, 1 ) 0.5 Tile.Dirt
  in
    div
    [ Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "75%" ) ]
    , Canvas.onMouseMove MouseMove
    , Canvas.onMouseDown MouseDown
    ]
    [ Game.render
      { time = 0
      , camera = model.camera
      , size = ( model.dimensions.width, model.dimensions.height  ) }
      ( renderGrid model )
    , ui
    ]
