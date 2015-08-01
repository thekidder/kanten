module Fps where

import Debug exposing (watch)

type alias Fps f =
  { f |
    counter: Int
  , totalFrames: Int
  , totalOverDeadline: Int
  , lastFps: Float
  , lastOverDeadline: Float
  }

initialFps: Fps {}
initialFps =
  { counter = 0
  , totalFrames = 0
  , totalOverDeadline = 0
  , lastFps = 0
  , lastOverDeadline = 0
  }

updateFps: Fps {} -> Float -> Fps {}
updateFps fps t =
  let over = if t > 33 then 1 else 0
  in if fps.counter == 100
    then
      { counter = 0
      , totalFrames = 0
      , totalOverDeadline = 0
      , lastFps = (toFloat fps.totalFrames) / (toFloat fps.counter)
        |> Debug.watch "avg ms"
      , lastOverDeadline = (toFloat fps.totalOverDeadline) / (toFloat fps.counter)
        |> Debug.watch "% frames over 33ms"
      }
    else
      { counter = fps.counter + 1
      , totalFrames = fps.totalFrames + t
      , totalOverDeadline = fps.totalOverDeadline + over
      , lastFps = fps.lastFps
      , lastOverDeadline = fps.lastOverDeadline
      }
