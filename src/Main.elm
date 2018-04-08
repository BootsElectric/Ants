module Main exposing (..)

-- Import Public Packages
import AnimationFrame
import Game.TwoD.Camera as Camera exposing ( Camera )
import Game.Resources as Resources exposing ( Resources )
import Html exposing ( program, div, text, Html )
import Keyboard.Extra
import Task
import Tuple exposing ( first, second )
import Window exposing ( Size )
-- Import Private Packages
import Ants exposing (..)
import Messages exposing ( Msg(..) )
import Model exposing (..)
import Tile exposing ( Tile )
import Textures exposing (..)
import View exposing ( view )


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoMessageYet ->
      ( model, Cmd.none )

    Resize size ->
      ( { model | dimensions = size }, Cmd.none )

    MouseMove point ->
      ( { model | mPosX = point.x, mPosY = point.y }, Cmd.none )

    Resources msg ->
      ( { model | resources = Resources.update msg model.resources }, Cmd.none )

    Keys msg ->
      ( { model | pressedKeys = Keyboard.Extra.update msg model.pressedKeys }, Cmd.none )

    MouseDown point ->
     let
        x =
          round point.x

        y =
          round point.y

        mousePos =
          Camera.viewportToGameCoordinates model.camera ( model.dimensions.width, model.dimensions.height ) ( x, y )

        normalX =
          first mousePos |> floor

        normalY =
          second mousePos |> floor

        tile =
          case Tile.getTile model.tiles normalX normalY of
            Just tile ->
              Just tile

            Nothing ->
              model.selected

      in

        ( { model
          | mPosX = point.x
          , mPosY = point.y
          , selected = tile
          }, Cmd.none )

    Tick dt ->
      let
        arrows =
          Keyboard.Extra.wasd model.pressedKeys

        width =
          ( first ( Camera.getViewSize ( toFloat model.dimensions.width, toFloat model.dimensions.height ) model.camera ) )

        camSpeed =
          dt * ( width / 4 )

        x =
          toFloat arrows.x

        y =
          toFloat arrows.y

        newCamera =
          if inBounds model camSpeed x y then
              Camera.moveBy ( x * camSpeed, y * camSpeed ) model.camera
          else
              model.camera
      in
        ( {  model
          | time = dt + model.time
          , camera = newCamera
          }, Cmd.none )
    Collect ->
      ( { model | ants = ( Ants.increase model.ants 10 ), selected = ( Tile.updateKind model.selected Tile.Dirt ) }, Cmd.none )


{-|
  Checks to ensure that the camera doesn't move too far from the grid of tiles
    model is the Model
    speed is the velocity of the camera's movement
    x is the X direction ( 1, 0 or -1 ) of the camera's movement
    y is the Y direction ( 1, 0 or -1 ) of the camera's movement
-}
inBounds : Model -> Float -> Float -> Float -> Bool
inBounds model speed x y =
  let
      newX =
        first ( Camera.getPosition model.camera ) + ( x * speed )

      newY =
        second ( Camera.getPosition model.camera ) + ( y * speed )

      maxX =
        ( toFloat ( List.length model.tiles ) ) / 10

      maxY =
        ( toFloat ( List.length model.tiles ) ) / 4

      checkX =
        newX > -maxX && newX < maxX

      checkY =
        newY > -maxY && newY < maxY
  in
    if checkX && checkY then
        True
    else
        False

-- SUBSCRIPTIONS TODO: try removing AnimationFrame
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize
    , Sub.map Keys Keyboard.Extra.subscriptions
    , AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
    ]


-- INIT
init : ( Model, Cmd Msg )
init =
  let
      dirt =
        getTexturePaths dirtUrl

      queen =
        getTexturePaths queenUrl

      food =
        getTexturePaths foodUrl

  in
    initialModel
    ! [ Task.perform Resize Window.size
      , Cmd.map Resources ( Resources.loadTextures  ( List.concat [ dirt, queen, food ] )  )
      ]


{-|
  Sets up the paths for each texture of a kind of tile: normal, highlighted and selected
    kind: a String representation of a Tile.Kind
-}
getTexturePaths : String -> List String
getTexturePaths kind =
    [ String.concat [ texturesUrl, kind ]
    , String.concat [ texturesUrl, "h-", kind ]
    , String.concat [ texturesUrl, "s-", kind ]
    ]


-- MAIN
main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
