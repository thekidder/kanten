module Collision where

import Debug exposing (watch)
import Math.Vector3 exposing (..)

import Circle exposing (Circle, circle)
import Util exposing (debugVec)


type Result = None | Collision Vec3


collision : Circle -> Circle -> Result
collision rhs lhs =
   let axis = separatingAxis rhs lhs
       projLhs = project lhs axis |> Debug.watch "lhs"
       projRhs = project rhs axis |> Debug.watch "rhs"
       mag = overlap projLhs projRhs
   in if mag > 0
     then Collision (scale mag axis)
     else None

separatingAxis : Circle -> Circle -> Vec3
separatingAxis rhs lhs =
  sub rhs.position lhs.position
    |> normalize
    |>debugVec "axis"

project: Circle -> Vec3 -> (Float, Float)
project circle axis =
  let projection = dot circle.position axis
  in (projection - circle.radius, projection + circle.radius)

overlap: (Float, Float) -> (Float, Float) -> Float
overlap lhs rhs =
  let (minRange, maxRange) = orderRanges lhs rhs
  in (snd minRange) - (fst maxRange)

orderRanges: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
orderRanges lhs rhs =
  if (fst lhs) <= (fst rhs)
    then (lhs, rhs)
    else (rhs, lhs)
