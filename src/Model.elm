module Model exposing ( Model, initialModel, Coord, Column )

import Tile exposing ( Tile, Kind(..), newTile )

import Window exposing (..)
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.Resources as Resources exposing ( Resources )
import Keyboard.Extra exposing ( Key(..) )

type alias Model =
  { tiles : List ( List Tile )
  , dimensions : Size
  , mPosX : Float
  , mPosY : Float
  , screen : ( Float, Float )
  , time : Float
  , camera : Camera
  , resources : Resources
  , pressedKeys : List Key
  , selected: Maybe Tile
  , queen: Tile
  }


initialModel : Model
initialModel =
   { tiles = generateTileGrid ( centreGrid 12 ( generateGrid 25 25 ) )
   , dimensions = Size 0 0
   , mPosX = 0
   , mPosY = 0
   , screen = ( 800, 600 )
   , time = 0
   , camera = Camera.fixedArea ( 32 * 20 ) ( 0, 0 )
   , resources = Resources.init
   , pressedKeys = []
   , selected = Nothing
   , queen = newTile ( 1, 12 ) Queen
  }


type alias Coord =
  ( Int, Int )


type alias Column =
  List Coord


generateTileGrid : List Column -> List ( List Tile )
generateTileGrid grid =
  List.map generateTileColumn grid



generateTileColumn : Column -> List Tile
generateTileColumn column =
  List.map generateTile column



generateTile : Coord -> Tile
generateTile coord =
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
