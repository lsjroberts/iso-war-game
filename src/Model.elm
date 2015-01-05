module Model where

import World.Model (World,demo,generate)

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
    { state = Play
    , world = generate 20 20 1
    , offset = (0,0) }