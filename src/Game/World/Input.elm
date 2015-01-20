module Game.World.Input where

import Signal

import Game.World.Model (Tile)

type TileAction
    = NoOp
    | MouseOver Tile
    | MouseOut Tile
    | Hover Tile
    | Click Tile



tileInput : Signal.Channel TileAction
tileInput = Signal.channel NoOp

worldInput : Signal WorldInput
worldInput = Signal.map WorldInput tileInput