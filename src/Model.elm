module Model where

import World.Model (..)

type State =
    Play | Pause

type alias Offset =
    (Float, Float)

type alias GameState =
    { state:State
    , world:World
    , offset:Offset }

defaultGame : GameState
defaultGame =
    let seed = 1
    in { state = Play
       , world = filled GrassTile 20 20 seed
               |> featureHill [(0,0),(1,1),(2,2),(3,2)] seed
       , offset = (0,0) }