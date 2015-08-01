module Fps where

import Debug exposing (watch)

type alias Fps f =
  { f |
    counter: Int
  , totalTime: Float
  , totalOverDeadline: Int
  , lastFps: Float
  , lastOverDeadline: Float
  }

initialFps: Fps {}
initialFps =
  { counter = 0
  , totalTime = 0
  , totalOverDeadline = 0
  , lastFps = 0
  , lastOverDeadline = 0
  }

-- deadline in ms for reporting high frame times
deadline = 17

-- number of frames to average stats over
numFrames = 20

updateFps: Fps {} -> Float -> Fps {}
updateFps fps t =
  let over = if t > deadline then 1 else 0
  in if fps.counter == numFrames
    then
      { counter = 0
      , totalTime = 0
      , totalOverDeadline = 0
      , lastFps = fps.totalTime / (toFloat fps.counter) |> Debug.watch "avg ms"
      , lastOverDeadline = (toFloat fps.totalOverDeadline) / (toFloat fps.counter)
          |> Debug.watch ("% frames over " ++ (toString deadline) ++ " ms")
      }
    else
      { counter = fps.counter + 1
      , totalTime = fps.totalTime + t
      , totalOverDeadline = fps.totalOverDeadline + over
      , lastFps = fps.lastFps
      , lastOverDeadline = fps.lastOverDeadline
      }
