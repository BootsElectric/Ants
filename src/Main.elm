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
import Dict exposing (..)
import Random exposing ( initialSeed, step, Generator )
-- Import Private Packages
import Ants exposing (..)
import Messages exposing ( Msg(..) )
import Model exposing (..)
import Tile exposing ( Tile, updateToDirt )
import Textures exposing (..)
import View exposing ( view )
import UnMaybe exposing ( unMaybeInt )


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
      PreGame ->
        case msg of
          Resize size ->
            ( { model | dimensions = size }, Cmd.none )

          UpdateState state ->
            ( { model | state = state }, Cmd.none )

          Resources msg ->
            ( { model | resources = Resources.update msg model.resources }, Cmd.none )

          Generate ->
            let
                floatList =
                  Random.list 625 (Random.float 0 1)
            in

              ( model, Random.generate ( Generated InGame ) floatList )

          Generated state floats ->
            let
                newFloats =
                  Model.randomFloatsFromLists (Model.listOfCoords 12 12 []) floats

                newGrid =
                  Model.createGrid model 12 12 Dict.empty
            in

              ( { model
                | randomFloats = newFloats
                , floatsList = floats
                , grid = newGrid
                , state = state
                }, Cmd.none )

          _ ->
            ( model, Cmd.none )

      InGame ->
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

              w =
                model.dimensions.width

              h =
                model.dimensions.height

              mousePos =
                Camera.viewportToGameCoordinates model.camera ( w, h ) ( x, y )

              normalX =
                first mousePos |> floor

              normalY =
                second mousePos |> floor

              tile =
                case Tile.getTile model.grid normalX normalY of
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

              w = model.dimensions.width

              h = model.dimensions.height


              width =
                ( first ( Camera.getViewSize ( toFloat w, toFloat h ) model.camera ) )

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
            let
                selX =
                  unMaybeInt <| Tile.getX model.selected

                selY =
                  unMaybeInt <| Tile.getY model.selected

                grid =
                  Dict.update ( selX, selY ) updateToDirt model.grid

            in

            ( { model
            | ants = ( Ants.increaseFood model.ants 200 )
            , grid = grid
            , selected = get ( selX, selY ) grid }, Cmd.none )

          UpdateState state ->
            ( { model | state = state }, Cmd.none )

          Dig ->
            let
              selX =
                unMaybeInt <| Tile.getX model.selected

              selY =
                unMaybeInt <| Tile.getY model.selected

              grid =
                Dict.update ( selX, selY ) Tile.digTile model.grid

            in
              ( { model
              | ants = ( Ants.decreaseFood model.ants 40 )
              , grid = grid
              , selected = get ( selX, selY ) grid }, Cmd.none )

          _ ->
            ( model, Cmd.none )

      PostGame ->
        case msg of
          Resize size ->
            ( { model | dimensions = size }, Cmd.none )

          UpdateState state ->
            ( { model
            | state = state
            , ants = initialAnts
            }
            , Cmd.none )

          _ ->
            ( model, Cmd.none )




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
        ( toFloat ( Dict.size model.grid ) ) / 400

      maxY =
        ( toFloat ( Dict.size model.grid ) ) / 100

      checkX =
        newX > -maxX && newX < maxX

      checkY =
        newY > -maxY && newY < maxY
  in
    if checkX && checkY then
        True
    else
        False


randomFloatList : Generator Float-> List Float -> Model -> Random.Seed -> Int -> List Float
randomFloatList generator floats model seed var =

  let
      floatSeedPair =
        step generator seed

      float =
        Tuple.first floatSeedPair

      newSeed = Tuple.second floatSeedPair
  in


  if var > 0 then
      List.concat [ floats,  [ float ], randomFloatList generator floats model newSeed ( var - 1 ) ]
  else
      floats



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

      undug=
        getTexturePaths undugUrl

  in
    initialModel
    ! [ Task.perform Resize Window.size
      , Cmd.map Resources ( Resources.loadTextures  ( List.concat [ dirt, queen, food, undug ] )  )
      , Random.generate ( Generated PreGame ) ( Random.list 625 ( Random.float 0 1 ) )
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
