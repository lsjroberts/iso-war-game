module Game.World.Input where

import Signal

import Game.World.Model (Tile)

type TileAction
    = NoOp
    | MouseOver Tile
    | MouseOut Tile
    | Hover Tile
    | Click Tile

tileActionChannel : Signal.Channel TileAction
tileActionChannel = Signal.channel NoOp

worldInput : Signal WorldInput
worldInput = Signal.map WorldInput tileActionChannel