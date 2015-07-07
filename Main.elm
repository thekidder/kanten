import AnimationFrame
import Debug
import Signal.Extra

import Keyboard exposing (arrows)
import Graphics.Element exposing (Element)
import Math.Vector3 exposing (..)
import Math.Vector3 as Vector3
import Math.Matrix4 exposing (Mat4, makeOrtho2D)
import Mouse exposing (position)
import Signal exposing (mergeMany, foldp)
import Time exposing (fps)
import WebGL exposing (webgl)
import Window exposing (dimensions)

import Circle
import Collision exposing (collision)
import Util exposing (Vertex, debugVec)

-- Setup scaffolding, model, and actions

main : Signal Element
main = Signal.map render update'

update' : Signal Model
update' = Signal.Extra.foldp' update getInitial updates

emptyModel =
  { viewportWidth = 4
  , mouseX = 0
  , dimens = (1280, 800)
  , t = 0
  , direction = vec3 0 0 0
  , impulse = vec3 0 0 0
  , velocity = vec3 0 0 0
  , self = Circle.circle 64 (vec3 1 0 0) (vec3 0 0 0) 0.6
  , obstacle = Circle.circle 64 (vec3 0 0 1) (vec3 1 0 0) 1.0
  , collision = False
  }

getInitial : Action -> Model
getInitial action =
  case action of
    MouseMove (x, y) ->
      { emptyModel | mouseX <- x }
    Move direction ->
      emptyModel
    WindowResize (w, h) ->
      { emptyModel | dimens <- (w, h) }
    TimeDelta t ->
      { emptyModel | t <- t }

type alias Model =
  { viewportWidth : Float
  , mouseX : Int
  , dimens : (Int, Int)
  , t: Float
  , direction: Vec3
  , impulse: Vec3
  , velocity: Vec3
  , self: Circle.Circle
  , obstacle: Circle.Circle
  , collision: Bool
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
    --, (Signal.map TimeDelta AnimationFrame.frame)
    , (Signal.map TimeDelta (fps 60))
    ]

damp velocity =
  scale 0.2 velocity |> Vector3.negate

updateVelocity model =
    model.impulse
    |> add model.velocity
    |> add (damp model.velocity)
    |> debugVec "vel"

updatePosition model =
  model.velocity
    |> scale (model.t / 1000)
    |> add model.self.position

doCollision model newPos =
  let (x, y, z) = toTuple newPos
      nextX = max -3.6 x |> min 3.6
      nextY = max -2.6 y |> min 2.6
  in vec3 nextX nextY z

updateSelf model =
  let self = model.self
  in { self | position <- updatePosition model
    |> doCollision model
    |> debugVec "pos" }

update : Action -> Model -> Model
update action model =
  case action of
    MouseMove (x, y) -> { model | mouseX <- x }
    Move direction -> { model | direction <- direction |> debugVec "dir" }
    WindowResize dimens ->
      { model |
        dimens <- dimens
      }
    TimeDelta t ->
      { model |
        t <- t
      , impulse <- scale 35 model.direction
        |> scale (model.t / 1000)
      , velocity <- updateVelocity model
      , self <- updateSelf model
      , collision <- Collision.collision model.self model.obstacle
        |> Debug.watch "collision"
      }

-- define rendering

getViewportWidth model = 0.6 / (0.2 + lerp (toFloat model.mouseX) 0 (toFloat (fst model.dimens)))

render : Model -> Element
render model =
  webgl model.dimens
    [ Circle.entity model.self (viewport model)
    , Circle.entity model.obstacle (viewport model)
    ]

lerp x min max = (x - min) / (max - min)

-- takes mouse pos and window dimens and outputs left, right, top, bottom viewport
viewport : Model -> Mat4
viewport model =
  let (w, h) = model.dimens
      x = model.mouseX
      width = model.viewportWidth
      aspect = toFloat h / toFloat w
  in makeOrtho2D  -width width (aspect * -width) (aspect * width)
