module Circle where

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Entity, Triangle, entity)

import Util exposing (Vertex, fragmentShader, vertexShader)

type alias Circle = { position: Vec3, radius: Float, renderable: List (Triangle Vertex) }

circle : Int -> Vec3 -> Vec3 -> Float -> Circle
circle n color position radius =
  let outer = sub color (vec3 0.2 0.2 0.2)
  in { position = position
     , radius = radius
     , renderable = renderable (vec3 0 0 0) n radius color outer
     }

entity : Circle -> Vec3 -> Mat4 -> Entity
entity circle pos perspective =
  WebGL.entity vertexShader fragmentShader circle.renderable { perspective = perspective, worldPos = add circle.position pos }

-- create circle mesh from center pos, num segments, radius, rotation
renderable : Vec3 -> Int -> Float -> Vec3 -> Vec3 -> List (Triangle Vertex)
renderable center n radius innerColor outerColor =
  renderable' center n radius innerColor outerColor 0 (2 * pi / (toFloat n)) []

renderable' : Vec3 -> Int -> Float -> Vec3 -> Vec3 -> Float -> Float -> List (Triangle Vertex) -> List (Triangle Vertex)
renderable' center n radius innerColor outerColor start increment current =
  let (x, y, z) = toTuple center
      s = segment center radius innerColor outerColor start increment
      total = s :: current
  in
    if n == 0 then
      total
    else
      renderable' center (n - 1) radius innerColor outerColor (start + increment) increment total

segment : Vec3 -> Float -> Vec3 -> Vec3 -> Float -> Float -> (Triangle Vertex)
segment center radius innerColor outerColor start width =
  let startPoint = fromPolar (radius, start) |> fromXY
      endPoint = fromPolar (radius, start + width) |> fromXY
  in
    ( Vertex center innerColor
    , Vertex (add center startPoint) outerColor
    , Vertex (add center endPoint) outerColor
    )

fromXY : (Float, Float) -> Vec3
fromXY xy =
  let (x, y) = xy
  in vec3 x y 0
