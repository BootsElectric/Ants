module View exposing ( view, isWithin )

import Html exposing ( Html, div )
import Html.Attributes as Attr exposing ( style )
import Tuple exposing ( first, second )
--
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.TwoD.Render as Render exposing ( Renderable, rectangle )
import Game.Resources as Resources exposing (Resources)
import ElementRelativeMouseEvents as Canvas exposing ( onMouseMove )
--
import Messages exposing ( Msg(..) )
import Model exposing ( Model, Column, Coord )
import Textures exposing (..)
import Tile exposing ( Tile, Kind(..), getX, getY )
import UI exposing (..)

{-|
  renders the grid of tiles
-}
renderGrid : Model -> Camera -> Resources -> List ( List Tile ) -> List Renderable
renderGrid model camera resources tileGrid =
  List.concat ( List.map ( renderColumn model camera resources ) tileGrid )


{-|
  Rednders a Column of tiles
-}
renderColumn : Model -> Camera -> Resources -> List Tile -> List Renderable
renderColumn model camera resources tiles =
  List.map ( renderCoord model camera resources ) tiles


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
      case tile.kind of
        Dirt ->
          useUrl model camera x y dirtUrl

        Queen ->
          useUrl model camera x y queenUrl

        Food ->
          useUrl model camera x y foodUrl
  in
    Render.sprite
      { texture = Resources.getTexture texture resources
      , position = position
      , size = ( 1, 1 )
      }


useUrl : Model -> Camera -> Int -> Int -> String -> String
useUrl model camera x y kind =
    if isWithin model camera x y then

      String.concat [texturesUrl, "h-", kind]

    else if isSelected model camera x y then

      String.concat [texturesUrl, "s-", kind]

    else

      String.concat [texturesUrl, kind]


isSelected : Model -> Camera -> Int -> Int -> Bool
isSelected m c x y =

  if Tile.getTile m.tiles x y == m.selected then
    True
  else
    False



{-|
  Uses Camera.viewportToGameCoordinates to normalise mouse position then compares that position to the position of the tiles.

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
        Camera.viewportToGameCoordinates camera ( m.dimensions.width, m.dimensions.height ) ( round m.mPosX, round m.mPosY )

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


view : Model -> Html Msg
view model =

  div
  [ Attr.style [ ( "overflow", "hidden" ), ( "width", "100%" ), ( "height", "75%" ) ]
  , Canvas.onMouseMove MouseMove
  , Canvas.onMouseDown MouseDown
  ]
  [ Game.render
    { time = 0, camera = model.camera, size = ( model.dimensions.width, model.dimensions.height  ) }
    ( renderGrid model model.camera model.resources model.tiles )
  , writeUI model
  ]
