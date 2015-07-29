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
    obj: GameObject {}
    , direction: Vec3
    , impulse: Vec3
    , velocity: Vec3
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
  let c = Circle.circle 64 (vec3 1 0 0) pos 1.0
  in
    { position = pos
    , collidables = [Collision.Circle c]
    , renderables = [Circle.entity c]
    }

movePlayer: Player {} -> Vec3 -> Player {}
movePlayer player direction =
  { player | direction <- direction |> debugVec "dir"}

updatePlayer: Player {} -> Float -> Player {}
updatePlayer player t =
  { player |
    impulse <- scale 35 player.direction |> scale (t / 1000) |> debugVec "impulse"
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

updateGameObject player t =
  let obj = player.obj
  in
    { obj |
      position <- player.velocity
        |> scale (t / 1000)
        |> add obj.position
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
