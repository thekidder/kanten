import AnimationFrame
import Debug
import Signal.Extra

import Keyboard exposing (arrows)
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
  { viewportWidth = 4
  -- , mouseX = 0
  , dimens = (1280, 800)
  , t = 0
  -- , player = Circle.circle 64 (vec3 1 0 0) (vec3 -1 0 0) 0.6
  , player = Player.create (vec3 0 0 0)
  -- , obstacle = Circle.circle 64 (vec3 0 0 1) (vec3 1 0 0) 1.0
  -- , square = Polygon.square (vec3 0 1 0) (vec3 -3 0 0) 1.0
  , gameObjects = [
    Obstacle.circle (vec3 0 0 0) (Circle.circle 64 (vec3 0 0 1) (vec3 0 1 0) 1.0)
  ]
  -- , collision = Collision.None
  , fps = { counter = 0, total = 0, last = 0, over = 0 }
  }

getInitial : Action -> Model
getInitial action =
  case action of
    MouseMove (x, y) ->
      emptyModel
    --   { emptyModel | mouseX <- x }
    Move direction ->
      emptyModel
    WindowResize (w, h) ->
      { emptyModel | dimens <- (w, h) }
    TimeDelta t ->
      { emptyModel | t <- t }

type alias Model =
  { viewportWidth : Float
  -- , mouseX : Int
  , dimens : (Int, Int)
  , t: Float
  , player: Player.Player {}
  -- , direction: Vec3
  -- , impulse: Vec3
  -- , velocity: Vec3
  , gameObjects: List (GameObject {})
  -- , collision: Collision.Result
  , fps: { counter: Int, total: Float, last: Float, over: Int }
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

updates : Signal Action
updates =
  mergeMany
  {- WindowResize needs to be first; ensures that the initial input to the
     foldp' is the current window dimensions
  -}
    [ (Signal.map WindowResize Window.dimensions)
    , (Signal.map MouseMove Mouse.position)
    , (Signal.map movementVector Keyboard.arrows)
    -- swap the following two lines to use AnimationFrame based timing or simple
    -- fps based timing
    , (Signal.map TimeDelta AnimationFrame.frame)
    --, (Signal.map TimeDelta (fps 60))
    ]

updateFps t fps =
  let over = if t > 33 then 1 else 0
  in if fps.counter == 100
    then { counter = 0, total = 0, last = (fps.total / (toFloat fps.counter)) |> Debug.watch "avg ms", over = 0 }
    else { counter = fps.counter + 1, total = fps.total + t, last = fps.last, over = fps.over + over |> Debug.watch "frames over 33ms" }

update : Action -> Model -> Model
update action model =
  case action of
    MouseMove (x, y) -> model -- { model | mouseX <- x }
    Move direction -> { model | player <- movePlayer model.player direction }
    WindowResize dimens ->
      { model |
        dimens <- dimens
      }
    TimeDelta t ->
      { model |
        t <- t
      -- , impulse <- scale 35 model.direction
      --   |> scale (model.t / 1000)
      -- , velocity <- updateVelocity model
      -- , self <- updateSelf model
      -- , collision <- Collision.collision model.self model.obstacle
      --   |> Debug.watch "collision"
      , player <- updatePlayer model.player t
      , fps <- updateFps t model.fps
      }

-- define rendering

-- getViewportWidth model = 0.6 / (0.2 + lerp (toFloat model.mouseX) 0 (toFloat (fst model.dimens)))

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

-- lerp x min max = (x - min) / (max - min)

-- takes mouse pos and window dimens and outputs left, right, top, bottom viewport
viewport : Model -> Mat4
viewport model =
  let (w, h) = model.dimens
      width = model.viewportWidth
      aspect = toFloat h / toFloat w
  in makeOrtho2D  -width width (aspect * -width) (aspect * width)
