module GameObject where

import Collision exposing (Shape)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Entity)

-- world pos -> perspective
type alias Renderable = Vec3 -> Mat4 -> Entity

type alias GameObject g =
  { g |
    position: Vec3
  , collidables: List (Shape)
  , renderables: List (Renderable)
  }
