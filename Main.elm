import AnimationFrame
import Debug
import Signal.Extra

import Keyboard exposing (arrows)
import Fps exposing (..)
import GameObject exposing (..)
import Graphics.Element exposing (Element)
import Math.Vector3 exposing (..)
import Math.Vector3 as Vector3
import Math.Matrix4 exposing (Mat4, makeOrtho2D)
import Mouse exposing (position)
import Obstacle exposing (..)
import Player exposing (..)
import Signal exposing (mergeMany, foldp)
import Time exposing (fps)
import WebGL exposing (webgl)
import Window exposing (dimensions)

import Circle
import Collision exposing (Result, collision)
import Polygon
import Util exposing (Vertex, debugVec)

-- Setup scaffolding, model, and actions

main : Signal Element
main = Signal.map render update'

update' : Signal Model
update' = Signal.Extra.foldp' update getInitial updates

emptyModel =
  { viewportWidth = 20
  , dimens = (1280, 800)
  , t = 0
  , player = Player.create (vec3 0 0 0)
  , gameObjects = [
    Obstacle.circle (vec3 0 0 0) (Circle.circle 64 (vec3 0 0 1) (vec3 0 1 0) 1.0)
  ]
  , fps = initialFps
  }

getInitial : Action -> Model
getInitial action =
  case action of
    MouseMove (x, y) ->
      emptyModel
    Move direction ->
      emptyModel
    WindowResize (w, h) ->
      { emptyModel | dimens <- (w, h) }
    TimeDelta t ->
      { emptyModel | t <- t }

type alias Model =
  { viewportWidth : Float
  , dimens : (Int, Int)
  , t: Float
  , player: Player.Player {}
  , gameObjects: List (GameObject {})
  , fps: Fps {}
  }

type Action =
  MouseMove (Int, Int) |
  Move Vec3 |
  WindowResize (Int, Int) |
  TimeDelta Float

movementVector: { x: Int, y: Int } -> Action
movementVector arrows =
  if arrows.x == 0 && arrows.y == 0
    then vec3 0 0 0 |> Move
    else vec3 (toFloat arrows.x) (toFloat arrows.y) 0
      |> normalize
      |> Move

-- use AnimationFrame based timing or simple fps based timing
useRequestAnimationFrame = True

updates : Signal Action
updates =
  let t = if useRequestAnimationFrame
    then (Signal.map TimeDelta AnimationFrame.frame)
    else (Signal.map TimeDelta (fps 60))
  in mergeMany
  {- WindowResize needs to be first; ensures that the initial input to the
     foldp' is the current window dimensions
  -}
    [ (Signal.map WindowResize Window.dimensions)
    , (Signal.map MouseMove Mouse.position)
    , (Signal.map movementVector Keyboard.arrows)
    , t
    ]

update : Action -> Model -> Model
update action model =
  case action of
    MouseMove (x, y) -> model
    Move direction -> { model | player <- movePlayer model.player direction }
    WindowResize dimens ->
      { model |
        dimens <- dimens
      }
    TimeDelta t ->
      { model |
        t <- t
      , player <- updatePlayer model.player t
      , fps <- updateFps model.fps t
      }

-- define rendering

gatherObjectRenderables: GameObject {} -> Model -> List (WebGL.Entity)
gatherObjectRenderables g model =
  List.map (\r -> r g.position (viewport model)) g.renderables

gatherRenderables: Model -> List (List (WebGL.Entity))
gatherRenderables model =
  let objRenderables = List.map (\g -> gatherObjectRenderables g model) model.gameObjects
      playerRenderables = [gatherObjectRenderables model.player.obj model]
  in objRenderables ++ playerRenderables

render : Model -> Element
render model =
  webgl model.dimens (List.foldl (++) [] (gatherRenderables model))

viewport : Model -> Mat4
viewport model =
  let (w, h) = model.dimens
      width = model.viewportWidth
      aspect = toFloat h / toFloat w
  in makeOrtho2D  -width width (aspect * -width) (aspect * width)
