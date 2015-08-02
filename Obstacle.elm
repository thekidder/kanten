module Obstacle where

import Math.Vector3 exposing (Vec3)

import Circle exposing (..)
import Collision exposing (..)
import GameObject exposing (..)

circle: Vec3 -> Circle -> GameObject {}
circle pos c =
  { position = pos
  , rotation = 0.0
  , collidables = [Collision.Circle c]
  , renderables = [Circle.entity c]
  }
