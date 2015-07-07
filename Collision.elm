module Collision where

import Debug exposing (watch)
import Math.Vector3 exposing (..)

import Circle exposing (Circle, circle)
import Util exposing (debugVec)


collision : Circle -> Circle -> Bool
collision rhs lhs =
   let axis = separatingAxis rhs lhs
       projLhs = project lhs axis |> Debug.watch "lhs"
       projRhs = project rhs axis |> Debug.watch "rhs"
   in overlap projLhs projRhs

separatingAxis : Circle -> Circle -> Vec3
separatingAxis rhs lhs =
  sub rhs.position lhs.position
    |> normalize
    |>debugVec "axis"

project: Circle -> Vec3 -> (Float, Float)
project circle axis =
  let projection = dot circle.position axis
  in (projection - circle.radius, projection + circle.radius)

overlap: (Float, Float) -> (Float, Float) -> Bool
overlap lhs rhs =
  let (minRange, maxRange) = orderRanges lhs rhs
  in (fst maxRange) >= (fst minRange) && (fst maxRange) <= (snd minRange)

orderRanges: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
orderRanges lhs rhs =
  if (fst lhs) <= (fst rhs)
    then (lhs, rhs)
    else (rhs, lhs)
