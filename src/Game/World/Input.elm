module Game.World.Input where

import Input (..)

worldPointerPos : Input -> (Int,Int)
worldPointerPos ({userInput} as input) =
    let (mx,my) = userInput.mousePos
    in (1,1)