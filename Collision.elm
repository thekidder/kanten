module Collision where

import Debug exposing (watch)
import Math.Vector3 exposing (..)

import Circle exposing (Circle)
import Polygon exposing (Polygon)
import Util exposing (debugVec)

type Shape = Circle Circle.Circle | Polygon Polygon.Polygon
type Result = None | Collision Vec3

maxFloat = 10000000.0

collision : Shape -> Shape -> Result
collision rhs lhs =
   let axes = separatingAxes rhs lhs ++ separatingAxes lhs rhs
       overlaps = List.map (\axis -> (axis, overlap (project lhs axis) (project rhs axis))) axes
       (axis, (flipped, leastOverlap)) = List.foldl minOverlap ((vec3 0 0 0), (False, maxFloat)) overlaps
       translationAxis = if not flipped then Math.Vector3.negate axis else axis
   in if leastOverlap > 0
     then Collision (scale leastOverlap translationAxis)
     else None

minOverlap : (Vec3, (Bool, Float)) -> (Vec3, (Bool, Float)) -> (Vec3, (Bool, Float))
minOverlap result1 result2 =
  let (axis1, (flipped1, overlap1)) = result1
      (axis2, (flipped2, overlap2)) = result2
  in if overlap1 <= overlap2
    then result1
    else result2

separatingAxes : Shape -> Shape -> List (Vec3)
separatingAxes lhs rhs =
  case lhs of
    Circle circle ->
      let v = vertices rhs
          minV = List.foldl (\v1 v2 -> if distanceSquared v1 circle.position < distanceSquared v2 circle.position then v1 else v2) (vec3 maxFloat maxFloat maxFloat) v
          t = toTuple minV |> Debug.watch "closest vert"
      in [sub circle.position minV |> normalize]
    Polygon polygon ->
      normals polygon

vertices: Shape -> List (Vec3)
vertices shape =
  case shape of
    Circle circle -> [circle.position]
    Polygon polygon -> List.map (add polygon.position) polygon.vertices

normals: Polygon -> List (Vec3)
normals polygon =
  case polygon.vertices of
    [] ->
      []
    first :: rest ->
      List.map2 normal (List.map (add polygon.position) polygon.vertices) (List.map (add polygon.position) (rest ++ [first]))

normal: Vec3 -> Vec3 -> Vec3
normal vert1 vert2 =
  let avg = add vert1 vert2 |> scale 0.5
      (x, y, z) = sub vert2 vert1 |> toTuple
      normal = fromTuple (-y, x, 0) |> normalize
  in normal

project: Shape -> Vec3 -> (Float, Float)
project shape axis =
  case shape of
    Circle circle ->
      let projection = dot circle.position axis
      in (projection - circle.radius, projection + circle.radius)
    Polygon polygon ->
      let verts = List.map (add polygon.position) polygon.vertices
          dots = List.map (dot axis) verts
          minDot = List.foldl min  maxFloat dots
          maxDot = List.foldl max -maxFloat dots
      in (minDot, maxDot)

overlap: (Float, Float) -> (Float, Float) -> (Bool, Float)
overlap lhs rhs =
  let (flipped, minRange, maxRange) = orderRanges lhs rhs
  in (flipped, (snd minRange) - (fst maxRange))

orderRanges: (Float, Float) -> (Float, Float) -> (Bool, (Float, Float), (Float, Float))
orderRanges lhs rhs =
  if (fst lhs) <= (fst rhs)
    then (True, lhs, rhs)
    else (False, rhs, lhs)
