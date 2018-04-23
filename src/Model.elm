module Model exposing ( Model, initialModel, Coord, Column, State(..)
  , randomFloatsFromLists, createCoordList, createGrid, listOfCoords )

import Tile exposing ( Tile, Kind(..), newTile, Grid )
import Ants exposing (..)
import UnMaybe exposing (..)

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
  , randomFloats : Dict ( Int, Int ) Float
  , floatsList : List Float
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
  , ants : Ants
  , undugTiles : Int
  , won : Bool
  }


{-|
  Sets up the initial model with default values
-}
initialModel : Model
initialModel =
   { state = PreGame
   , randomFloats = empty
   , floatsList = []
   , grid = empty
   , dimensions = Size 0 0
   , mPosX = 0
   , mPosY = 0
   , screen = ( 800, 600 )
   , time = 0
   , camera = Camera.fixedArea ( 32 * 20 ) ( 0, 0 )
   , resources = Resources.init
   , pressedKeys = []
   , selected = Nothing
   , ants = Ants.initialAnts
   , undugTiles = 624
   , won = False
  }


type alias Coord =
  ( Int, Int )


type alias Column =
  List Coord

{-|
  A function to generate a Dict of ( Int, Int ) and Float from List ( Int, Int ) and List FLoat
    coords - the List of ( Int, Int )
    floats - the List of Floats
-}
randomFloatsFromLists : List ( Int, Int ) -> List Float -> Dict ( Int, Int ) Float
randomFloatsFromLists coords floats =
    List.map2 (,) coords floats |> fromList

{-|
  Creates a List ( Int, Int ) from two Ints and an empy List
    var - the variable for recursion
    const - the constant for recursion should be the same as var
    list - the empty list to start from

  *NOTE* This is a helper function for randomFloatsFromLists
-}
listOfCoords : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
listOfCoords var const list =

    let
        newList =
          List.concat [ list, createCoordList var const ]
    in
        if var <= -const then
            newList
        else
            listOfCoords ( var - 1 ) const newList


{-|
  Creates a List ( Int, Int ) from two Ints
    x - the constant
    y - the variable

  *NOTE* This is a helper function of listOfCoords
-}
createCoordList : Int -> Int -> List ( Int, Int )
createCoordList x y =
    List.map (createCoord x) ( List.range -y y )

{-|
  Create a ( Int, Int ) from two Ints
    x - the x coordinate
    y - the y coordinate

  *NOTE* This is a helper function of createCoordList
-}
createCoord : Int -> Int -> ( Int, Int )
createCoord x y =
    ( x, y )

{-|
  Creates a Dict as a representation of a grid of Tiles
    var - the variable for recursion that will range from const to -const
    const - the constant for recursion that var will be compared against
    grid - the inital grid to start from

  var and const should be equal so that this function recurses the correct number of times.
  grid should be Dict.empty
-}
createGrid : Model -> Int -> Int -> Grid -> Grid
createGrid model var const grid =

  let
      newGrid =
        union grid ( fromList <| createPairList model var const )
  in

    if var <= -const then
      newGrid
    else
      createGrid model ( var - 1 ) const newGrid


{-|
  Creates a List of key value pairs that correspond to one column of a grid of coordinates
    x - the x coordinate of the column
    y - the maximum y value to create the list from ( the list will range from y to -y )
  *NOTE* This is a helper funtion for createGrid
-}
createPairList : Model -> Int -> Int -> List ( ( Int, Int ), Tile )
createPairList model x y =
    List.map (createKeyValuePair model x) ( List.range -y y )


{-|
  Creates a key value pair from a tuple of integers and a tile with coordinates corrisponding to that tuple
    x - the X coordinate of the tile
    y - the Y coordinate of the tile

  *NOTE* This is a helper function of createPairList
-}
createKeyValuePair : Model -> Int -> Int -> ( ( Int, Int ), Tile )
createKeyValuePair model x y =
    ( ( x, y ), generateTile model ( x, y ) )

{-|
  Creates a tile at random unless the coord is ( 0, 0 ) in which case a queen is created.
  Also there is a 10% chance for a tile to either be food or a disaster.
    model - the model to get the random list of floats from
    coord - The coordinates to create the tile at
-}
generateTile : Model -> Coord -> Tile
generateTile model coord =
  if coord == ( 0, 0 ) then
    let
        tile =
          newTile coord 0 Queen
    in
        { tile | isDug = True }


  else
    let
        float =
          unMaybeFloat <| get coord model.randomFloats
    in

      if float < 0.1 then

        newTile coord float Food

      else if float > 0.9 then

        newTile coord float Disaster

      else

        newTile coord float Dirt
