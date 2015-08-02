module Polygon where

import List exposing (head, tail)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Entity, Triangle, entity)

import Util exposing (Vertex, fragmentShader, vertexShader)

type alias Polygon = { position: Vec3, vertices: List (Vec3), renderable: List (Triangle Vertex) }

square : Vec3 -> Vec3 -> Float -> Polygon
square color position sideLen =
  let outer = sub color (vec3 0.2 0.2 0.2)
      halfSide = sideLen / 2
      vertices =
        [ vec3 -halfSide -halfSide 0
        , vec3 -halfSide  halfSide 0
        , vec3  halfSide  halfSide 0
        , vec3  halfSide -halfSide 0
        ]
  in { position = position
     , vertices = vertices
     , renderable = renderable vertices color outer
     }

entity : Polygon -> Mat4 -> Entity
entity polygon perspective =
  let uniforms =
    { perspective = perspective
    , localPos = (vec3 0 0 0)
    , worldPos = polygon.position
    , rotation = 0
    }
  in WebGL.entity vertexShader fragmentShader polygon.renderable uniforms

-- create polygon mesh
renderable : List (Vec3) -> Vec3 -> Vec3 -> List (Triangle Vertex)
renderable vertices innerColor outerColor =
  case vertices of
    [] ->
      []
    first :: rest ->
      let l = rest ++ [first]
      in renderable' l (center vertices) innerColor outerColor first []

renderable' : List (Vec3) -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex) -> List (Triangle Vertex)
renderable' vertices center innerColor outerColor lastVertex triangles =
  case vertices of
    [] ->
      triangles
    first :: rest ->
      renderable' rest center innerColor outerColor first ((segment center lastVertex first innerColor outerColor) :: triangles)

segment : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Triangle Vertex)
segment center lastVertex nextVertex innerColor outerColor =
  ( Vertex center innerColor
  , Vertex lastVertex outerColor
  , Vertex nextVertex outerColor
  )

center : List (Vec3) -> Vec3
center vertices = center' vertices 0 (vec3 0 0 0)

center' : List (Vec3) -> Int -> Vec3 -> Vec3
center' vertices num accumulator =
  case vertices of
    [] ->
      scale (1 / (toFloat num)) accumulator
    first :: rest ->
      center' rest (num + 1) (add accumulator first)
