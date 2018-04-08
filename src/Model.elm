module Model exposing ( Model, initialModel, Coord, Column )

import Tile exposing ( Tile, Kind(..), newTile, Grid )
import Ants exposing (..)

import Dict exposing (..)
import Window exposing (..)
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.Resources as Resources exposing ( Resources )
import Keyboard.Extra exposing ( Key(..) )

type alias Model =
  { grid : Grid
  , dimensions : Size
  , mPosX : Float
  , mPosY : Float
  , screen : ( Float, Float )
  , time : Float
  , camera : Camera
  , resources : Resources
  , pressedKeys : List Key
  , selected : Maybe Tile
  , queen : Maybe Tile
  , ants : Ants
  }


initialModel : Model
initialModel =
  let
    grid =
      createGrid 12 12 empty

  in
   { grid = grid
   , dimensions = Size 0 0
   , mPosX = 0
   , mPosY = 0
   , screen = ( 800, 600 )
   , time = 0
   , camera = Camera.fixedArea ( 32 * 20 ) ( 0, 0 )
   , resources = Resources.init
   , pressedKeys = []
   , selected = Nothing
   , queen = Tile.getTile grid 0 0
   , ants = Ants.initialAnts
  }


type alias Coord =
  ( Int, Int )


type alias Column =
  List Coord

createGrid : Int -> Int -> Grid -> Grid
createGrid var const grid =

  let
      newGrid =
        union grid ( fromList <| createPairList var const )
  in

    if var <= -const then
      newGrid
    else
      createGrid ( var - 1 ) const newGrid

createPairList : Int -> Int -> List ( ( Int, Int ), Tile )
createPairList x y =
    List.map (createKeyValuePair x) ( List.range -y y )

createKeyValuePair : Int -> Int -> ( ( Int, Int ), Tile )
createKeyValuePair x y =
    ( ( x, y ), generateTile ( x, y ) )

{-|
  Populates the grid with tiles
    grid - The grid to populate
-}
generateTileGrid : List Column -> List ( List Tile )
generateTileGrid grid =
  List.map generateTileColumn grid


{-|
  Poulates a column with tiles
    column - The column to populate
-}
generateTileColumn : Column -> List Tile
generateTileColumn column =
  List.map generateTile column


{-|
  Creates a tile at random unless the coord is ( 0, 0 ) in which case a queen is created
    coord - The coordinates to create the tile at
-}
generateTile : Coord -> Tile
generateTile coord =
  if coord == ( 0, 0 ) then

    newTile coord Queen

  else

      if coord == ( 1, 1 ) then

        newTile coord Food

      else

        newTile coord Dirt



{-|
  Centres the grid on 0, 0
    x is half the size of the grid
    grid is the grid to be centred
-}
centreGrid : Int -> List Column -> List Column
centreGrid x grid =
  List.map ( centreColumn x ) grid


{-|
  Centres a single column on the x axis and is shifted to be centred along the y axis
    x is half the size of the grid
    column is the column to be centred
-}
centreColumn : Int -> Column -> Column
centreColumn x column =
    List.map ( centreCoord x ) column


{-|
  Shifts the x and y coordinate of a Coord with the overall effect of centering the grid
    x is the amount to shift by
    coord is the Coord to shift
-}
centreCoord : Int -> Coord -> Coord
centreCoord x coord =
  ( ( Tuple.first coord ) - x, ( Tuple.second coord ) - x )



{-|
  Generates a List of Columns using generateColumn

    x is the width of the grid
    y is the height of the grid
-}
generateGrid : Int -> Int -> List Column
generateGrid x y =
  List.map ( generateColumn x ) ( List.range 0 ( y - 1 ) )


{-|
  Generates a Column of Coord's

    x is the variable
    y is the constant
-}
generateColumn : Int -> Int -> List Coord
generateColumn x y =
  List.indexedMap (,) ( List.repeat x y )
