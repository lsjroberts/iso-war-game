module Game.World.Updates where

import Signal

import Game.World.Model (Tile)

type TileAction
    = NoOp
    | MouseOver Tile
    | MouseOut Tile
    | Hover Tile
    | Click Tile

tileUpdates : Signal.Channel TileAction
tileUpdates = Signal.channel NoOp