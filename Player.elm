module Player where

import Math.Vector3 exposing (..)

import Circle exposing (..)
import Collision exposing (..)
import GameObject exposing (..)
import Util exposing (..)
--
-- create: GameObject
-- create =

type alias Player p =
  { p |
    direction: Vec3
  , impulse: Vec3
  , velocity: Vec3
  , obj: GameObject {}
  }

create: Vec3 -> Player {}
create pos =
  { direction = vec3 0 0 0
  , impulse = vec3 0 0 0
  , velocity = vec3 0 0 0
  , obj = create' pos
  }

create': Vec3 -> GameObject {}
create' pos =
  let c = Circle.circle 64 (vec3 0.4 0.67 1) (vec3 0 0 0) 1.0
      eye = Circle.circle 8 (vec3 1 1 1) (vec3 0.8 0 0) 0.1
  in
    { position = pos
    , rotation = 0.0
    , collidables = [Collision.Circle c]
    , renderables = [Circle.entity c, Circle.entity eye]
    }

movePlayer: Player {} -> Vec3 -> Player {}
movePlayer player direction =
  { player | direction <- direction |> debugVec "dir"}

updatePlayer: Player {} -> Float -> Player {}
updatePlayer player t =
    { player |
      impulse <- scale 55 player.direction |> scale (t / 1000) |> debugVec "impulse"
    , velocity <- updateVelocity player
    , obj <- updateGameObject player t
    }


damp velocity =
  scale 0.08 velocity |> Math.Vector3.negate

updateVelocity player =
    player.impulse
    |> add player.velocity
    |> add (damp player.velocity)
    |> debugVec "vel"

getRotation player =
  if player.velocity == (vec3 0 0 0)
    then player.obj.rotation
    else let (x, y, z) = toTuple player.velocity
             (r, theta) = toPolar (x, y)
         in theta

updateGameObject player t =
  let obj = player.obj
      rotation = getRotation player
  in
    { obj |
      position <- player.velocity
        |> scale (t / 1000)
        |> add obj.position
    , rotation <- rotation
    }


doCollision model newPos =
  let (x, y, z) = toTuple newPos
      nextX = max -3.6 x |> min 3.6
      nextY = max -2.6 y |> min 2.6
  in vec3 nextX nextY z

-- update player model =
--   let newPlayer = { player | position <-
--         updatePosition model
--           -- |> doCollision model
--           |> debugVec "pos" }
--   in case Collision.collision (Collision.Circle newPlayer) (Collision.Polygon model.square) of
--     Collision.Collision mtv -> { newPlayer |
--       position <- add newPlayer.position (debugVec "mtv" mtv) }
--     Collision.None -> newPlayer
