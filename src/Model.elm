module Model exposing ( Model, initialModel, Coord, Column, State(..) )

import Tile exposing ( Tile, Kind(..), newTile, Grid )
import Ants exposing (..)

import Dict exposing (..)
import Window exposing (..)
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.Resources as Resources exposing ( Resources )
import Keyboard.Extra exposing ( Key(..) )

type State
  = PreGame
  | InGame
  | PostGame

type alias Model =
  { state : State
  , grid : Grid
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
   { state = PreGame
   , grid = grid
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

{-|
  Creates a Dict as a representation of a grid of Tiles
    var - the variable for recursion that will range from const to -const
    const - the constant for recursion that var will be compared against

  var and const should be equal so that this function recurses the correct number of times

  *NOTE* This is a helper function for initialModel
-}
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


{-|
  Creates a List of key value pairs that correspond to one column of a grid of coordinates
    x - the x coordinate of the column
    y - the maximum y value to create the list from ( the list will range from y to -y )
  *NOTE* This is a helper funtion for createGrid
-}
createPairList : Int -> Int -> List ( ( Int, Int ), Tile )
createPairList x y =
    List.map (createKeyValuePair x) ( List.range -y y )


{-|
  Creates a key value pair from a tuple of integers and a tile with coordinates corrisponding to that tuple
    x - the X coordinate of the tile
    y - the Y coordinate of the tile

  *NOTE* This is a helper function of createPairList
-}
createKeyValuePair : Int -> Int -> ( ( Int, Int ), Tile )
createKeyValuePair x y =
    ( ( x, y ), generateTile ( x, y ) )

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
