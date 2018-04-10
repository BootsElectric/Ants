module Tile exposing (..)

import Dict exposing (..)

type alias Grid =
  Dict ( Int, Int ) Tile

{-|
  The Tile is an abstract representation of a gameboard tile

    coord: The coordinates of the tile in the game space
    kind: The kind of tile that this is
-}
type alias Tile =
  { coord : ( Int, Int )
  , kind : Kind
  , isDug : Bool
  , float : Float
  }

{-|
  The kind of tile that this is
-}
type Kind
  = Dirt
  | Queen
  | Food

{-|
  Creates a new tile with kind k at coordinates c

    c: a Tuple of Ints representing the tile's coordinates
    k: the kind of tile to make
-}
newTile : ( Int, Int ) -> Float  -> Kind -> Tile
newTile c f k =
  { coord = c
  , kind = k
  , isDug = False
  , float = f
  }


{-|
  Gets the value of the X coordinate of the given tile

    t: The tile to find the X coordinate of
-}
getX : Maybe Tile -> Maybe Int
getX t =
  case t of
    Just tile ->
     Just <| Tuple.first tile.coord

    Nothing ->
      Nothing

{-|
  Gets the value of the Y coordinate of the given tile

    t: The tile to find the Y coordinate of
-}
getY : Maybe Tile -> Maybe Int
getY t =
  case t of
    Just tile ->
     Just <| Tuple.second tile.coord

    Nothing ->
      Nothing


getFloat : Maybe Tile -> Maybe Float
getFloat t =
    case t of
        Just tile ->
            Just tile.float

        Nothing ->
            Nothing

{-|
  Gets the kind of tile that this is as a String

    tile: The tile to find the kind of
-}
getKind : Maybe Tile -> String
getKind tile =
  case tile of
      Just tile ->
        if tile.isDug == False then

          "Dig to\nfind out"

        else

          case tile.kind of
              Dirt ->
                "Dirt"

              Queen ->
                "Queen"

              Food ->
                "Food"

      Nothing ->
        ""



{-|
  Updates the kind of tile that this is

    tile: The tile to update
    kind: The kind to update this tile to
-}
updateKind : Maybe Tile -> Kind -> Maybe Tile
updateKind tile kind =
  case tile of
    Just tile ->

      Just { tile | kind = kind }

    Nothing ->

      Nothing

updateToDirt : Maybe Tile -> Maybe Tile
updateToDirt tile =
  case tile of
      Just tile ->
          Just { tile | kind = Dirt }

      Nothing ->
          Nothing


digTile : Maybe Tile -> Maybe Tile
digTile tile =
    case tile of
        Just tile ->
            Just { tile | isDug = True }

        Nothing ->
            Nothing



{-|
  Gets a tile specified by integer coordinates from a List of Lists of Tiles

    tiles: The grid of tiles to get the desired tile from
    x: The X coordinate of the desired tile
    y: The Y coordinate of the desired tile
-}
getTile : Grid -> Int -> Int -> Maybe Tile
getTile tiles x y =
  case get ( x, y ) tiles of
    Just tile ->
      Just tile
    Nothing ->
      Nothing
