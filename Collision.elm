module Collision where

import Debug exposing (watch)
import Math.Vector3 exposing (..)

import Circle exposing (Circle)
import Polygon exposing (Polygon)
import Util exposing (debugVec)

type Shape = Circle Circle.Circle | Polygon Polygon.Polygon
type Result = None | Collision Vec3

type alias Axis = Vec3
type alias RangeOverlap = { hasOverlap: Bool, overlap: Float }
type alias Range = (Float, Float)

maxFloat = 10000000.0
initialMinTranslation = vec3 maxFloat maxFloat maxFloat

collision : Shape -> Shape -> Result
collision rhs lhs =
   let axes = separatingAxes rhs lhs ++ separatingAxes lhs rhs
       overlaps = List.map (overlap lhs rhs) axes
       allOverlap = List.all (\(axis, overlap) -> overlap.hasOverlap) overlaps
   in if allOverlap
     then Collision   (List.foldl minTranslationVector initialMinTranslation overlaps)
     else None

minTranslationVector : (Axis, RangeOverlap) -> Vec3 -> Vec3
minTranslationVector (axis, overlap) rhs =
  let lhs = scale overlap.overlap axis
  in if lengthSquared lhs <= lengthSquared rhs
    then lhs
    else rhs

overlap: Shape -> Shape -> Axis -> (Axis, RangeOverlap)
overlap lhs rhs axis =
  (axis, overlap' (project lhs axis) (project rhs axis))

overlap': Range -> Range -> RangeOverlap
overlap' lhs rhs =
  -- it's easier to order the ranges to find the overlap, but we want to
  -- preserve the sign so that we can properly calculate the minimum translation
  -- vector
  let (flipped, minRange, maxRange) = orderRanges lhs rhs
      magnitude = (snd minRange) - (fst maxRange)
      hasOverlap = magnitude > 0
      sign = if flipped then 1 else -1
  in { hasOverlap = hasOverlap, overlap = sign * magnitude }

-- return a tuple of (flipped, minRange, maxRange)
orderRanges: Range -> Range -> (Bool, Range, Range)
orderRanges lhs rhs =
  if (fst lhs) <= (fst rhs)
    then (True, lhs, rhs)
    else (False, rhs, lhs)

project: Shape -> Vec3 -> Range
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

separatingAxes : Shape -> Shape -> List (Axis)
separatingAxes lhs rhs =
  case lhs of
    Circle circle ->
      let closest = closestVertex circle.position (vertices rhs)
      in [sub circle.position closest |> normalize]
    Polygon polygon ->
      normals polygon

closestVertex: Vec3 -> List (Vec3) -> Vec3
closestVertex target vertices =
  List.foldl (closestVertex' target) initalClosestVertex vertices

initalClosestVertex = vec3 maxFloat maxFloat maxFloat

closestVertex': Vec3 -> Vec3 -> Vec3 -> Vec3
closestVertex' target v1 v2 =
   if distanceSquared v1 target < distanceSquared v2 target then v1 else v2

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
      -- overlap the list of vertices with the list of vertices offset by one
      -- to get a list of edges
      let firstEdgeVerts = (List.map (add polygon.position) polygon.vertices)
          secondEdgeVerts = (List.map (add polygon.position) (rest ++ [first]))
      in List.map2 normal firstEdgeVerts secondEdgeVerts

normal: Vec3 -> Vec3 -> Vec3
normal vert1 vert2 =
  let avg = add vert1 vert2 |> scale 0.5
      (x, y, z) = sub vert2 vert1 |> toTuple
      normal = fromTuple (-y, x, 0) |> normalize
  in normal
