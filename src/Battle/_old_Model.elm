module Game.Model where

import Game.World.Model (..)

type alias Offset =
    (Float, Float)

type alias Game =
    { world:World
    , offset:Offset }

defaultGame : Game
defaultGame =
    { world = filled GrassTile 20 20 1
    , offset = (0,0) }