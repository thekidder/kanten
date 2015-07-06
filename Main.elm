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
import WebGL exposing (Triangle, Shader, entity, webgl)
import Window exposing (dimensions)


-- Define geometry and mesh functions

type alias Vertex = { position : Vec3, color : Vec3 }


fromXY : (Float, Float) -> Vec3
fromXY xy =
  let (x, y) = xy
  in vec3 x y 0


-- create circle mesh from center pos, num segments, radius, rotation
circle : Vec3 -> Float -> Float -> Vec3 -> Vec3 -> List (Triangle Vertex)
circle center n radius innerColor outerColor =
  circle' center n radius innerColor outerColor 0 (2 * pi / n) []


circleSegment : Vec3 -> Float -> Vec3 -> Vec3 -> Float -> Float -> (Triangle Vertex)
circleSegment center radius innerColor outerColor start width =
  let startPoint = fromPolar (radius, start) |> fromXY
      endPoint = fromPolar (radius, start + width) |> fromXY
  in
    ( Vertex center innerColor
    , Vertex (Vector3.add center startPoint) outerColor
    , Vertex (Vector3.add center endPoint) outerColor
    )


circle' : Vec3 -> Float -> Float -> Vec3 -> Vec3 -> Float -> Float -> List (Triangle Vertex) -> List (Triangle Vertex)
circle' center n radius innerColor outerColor start increment current =
  let (x, y, z) = Vector3.toTuple center
      segment = circleSegment center radius innerColor outerColor start increment
      total = segment :: current
  in
    if n == 0 then
      total
    else
      circle' center (n - 1) radius innerColor outerColor (start + increment) increment total


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
  , position = vec3 0 0 0
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
  , position: Vec3
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
    , (Signal.map TimeDelta AnimationFrame.frame)
    ]

debugVec: String -> Vec3 -> Vec3
debugVec name vec =
  let (x, y, z) = toTuple vec
  in Debug.watch name (x, y, z) |> fromTuple


damp velocity =
  scale 0.1 velocity |> Vector3.negate


updateVelocity model =
    model.impulse
    |> add model.velocity
    |> add (damp model.velocity)
    |> debugVec "vel"


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
      , impulse <- scale 10 model.direction
        |> scale (model.t / 1000)
      , velocity <- updateVelocity model
      , position <- model.velocity
        |> scale (model.t / 1000)
        |> add model.position
        |> debugVec "pos"
        --viewportWidth <- (0.1 * (getViewportWidth model)) + (0.9 * model.viewportWidth)
      }


-- define rendering

getViewportWidth model = 0.6 / (0.2 + lerp (toFloat model.mouseX) 0 (toFloat (fst model.dimens)))

render : Model -> Element
render model =
  let m = Debug.watch "model" model
      c = circle model.position 64 0.6 (vec3 1 0 0 ) (vec3 0.8 0 0)

  in
    webgl m.dimens
      [ entity vertexShader fragmentShader c { perspective = viewport m } ]


lerp x min max = (x - min) / (max - min)


-- takes mouse pos and window dimens and outputs left, right, top, bottom viewport
viewport : Model -> Mat4
viewport model =
  let (w, h) = model.dimens
      x = model.mouseX
      width = model.viewportWidth
      aspect = toFloat h / toFloat w
  in makeOrtho2D  -width width (aspect * -width) (aspect * width)


-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 } { unif | perspective:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 vcolor;

void main () {
    gl_Position = perspective * vec4(position, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
