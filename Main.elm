import AnimationFrame
import Debug
import Signal.Extra

import Graphics.Element exposing (Element)
import Math.Vector3 exposing (Vec3, vec3)
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
circle : Vec3 -> Float -> Float -> Float -> List (Triangle Vertex)
circle center n radius rot = circle' center n radius rot (2 * pi / n) []


circleSegment : Vec3 -> Float -> Float -> Float -> (Triangle Vertex)
circleSegment center radius start width =
  let startPoint = fromPolar (radius, start) |> fromXY
      endPoint = fromPolar (radius, start + width) |> fromXY
  in
    ( Vertex center (vec3 1 0 0)
    , Vertex (Vector3.add center startPoint) (vec3 0 1 0)
    , Vertex (Vector3.add center endPoint) (vec3 0 0 1)
    )


circle' : Vec3 -> Float -> Float -> Float -> Float -> List (Triangle Vertex) -> List (Triangle Vertex)
circle' center n radius start increment current =
  let (x, y, z) = Vector3.toTuple center
      segment = circleSegment center radius start increment
      total = segment :: current
  in
    if n == 0 then
      total
    else
      circle' center (n - 1) radius (start + increment) increment total


-- Setup scaffolding, model, and actions

main : Signal Element
main = Signal.map render update'


update' : Signal Model
update' = Signal.Extra.foldp' update getInitial updates


getInitial : Action -> Model
getInitial action =
  case action of
    MouseMove (x, y) ->
      { viewportWidth = 0, mouseX = x, dimens = (1280, 800), t = 0 }
    WindowResize (w, h) ->
      { viewportWidth = 0, mouseX = 0, dimens = (w, h), t = 0 }
    TimeDelta t ->
      { viewportWidth = 0, mouseX = 0, dimens = (1280, 800), t = 0 }


type alias Model =
  { viewportWidth : Float
  , mouseX : Int
  , dimens : (Int, Int)
  , t: Float
  }


type Action = MouseMove (Int, Int) | WindowResize (Int, Int) | TimeDelta Float


updates : Signal Action
updates =
  mergeMany
    [ (Signal.map WindowResize Window.dimensions)
    , (Signal.map MouseMove Mouse.position)
    , (Signal.map TimeDelta (Signal.foldp (+) 0 AnimationFrame.frame))
    ]

update : Action -> Model -> Model
update action model =
  case action of
    MouseMove (x, y) -> { model | mouseX <- x }
    WindowResize dimens ->
      { model |
        dimens <- dimens
      }
    TimeDelta t ->
      { model |
        t <- t,
        viewportWidth <- (0.1 * (getViewportWidth model)) + (0.9 * model.viewportWidth)
      }


-- define rendering

getViewportWidth model = 0.6 / (0.2 + lerp (toFloat model.mouseX) 0 (toFloat (fst model.dimens)))

render : Model -> Element
render model =
  let m = Debug.watch "model" model
      c = circle (vec3 0 0 0) 256 0.6 (m.t / 4000)

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
