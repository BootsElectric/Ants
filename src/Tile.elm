module Tile exposing ( Tile, Kind(..), newTile, getX, getY, getTile, getKind, updateKind, isTile )

{-|
  The Tile is an abstract representation of a gameboard tile

    coord: The coordinates of the tile in the game space
    kind: The kind of tile that this is
-}
type alias Tile =
  { coord : ( Int, Int )
  , kind : Kind
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
newTile : ( Int, Int ) -> Kind -> Tile
newTile c k =
  { coord = c
  , kind = k
  }


{-|
  Gets the value of the X coordinate of the given tile

    t: The tile to find the X coordinate of

    TODO: replace with Maybe.map
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


{-|
  Gets the kind of tile that this is as a String

    tile: The tile to find the kind of
-}
getKind : Maybe Tile -> String
getKind tile =
  case tile of
      Just tile ->
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


{-|
  Gets a tile specified by integer coordinates from a List of Lists of Tiles

    tiles: The grid of tiles to get the desired tile from
    x: The X coordinate of the desired tile
    y: The Y coordinate of the desired tile
-}
getTile : List ( List Tile ) -> Int -> Int -> Maybe Tile
getTile tiles x y =
  case List.head tiles of

    Just column ->

      case getTileFromColumn column x y of

        Just tile ->
          Just tile

        Nothing ->
          case List.tail tiles of
            Just newTiles ->
              getTile newTiles x y

            Nothing ->
              Nothing

    Nothing ->
      Nothing


{-|
  Gets a tile specified by integer coordinates from a List of Tiles

    tiles: The column of tiles to get the desired tile from
    x: The X coordinate of the desired tile
    y: The Y coordinate of the desired tile

  *NOTE*: This is a helper function of getTile
-}
getTileFromColumn : List Tile -> Int -> Int -> Maybe Tile
getTileFromColumn tiles x y =
  case List.head tiles of

    Just tile ->

      case isTile tile x y of

        Just tile ->
          Just tile

        Nothing ->
          case List.tail tiles of
            Just newTiles ->
              getTileFromColumn newTiles x y

            Nothing ->
              Nothing

    Nothing ->
      Nothing



{-|
  Checks to see if the candidate tile is the desired tile and returns it if so.

    tile: The candidate tile
    x: The X coordinate of the desired tile
    y: The Y coordinate of the desired tile

  *NOTE*: This is a helper function of getTileFromColumn
-}
isTile : Tile -> Int -> Int -> Maybe Tile
isTile tile x y =
  let
      tileX =
        Tuple.first tile.coord

      tileY =
        Tuple.second tile.coord
  in

    if tileX == x && tileY == y then
      Just tile
    else
      Nothing
