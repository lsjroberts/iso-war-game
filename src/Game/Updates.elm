module Game.Updates where

import Input (Input)
import Model (GameState,Offset)

stepPlay : Input -> GameState -> GameState
stepPlay ({delta,userInput} as input) ({offset} as gameState) =
    let offset' = offset |> stepOffset input
    in { gameState | offset <- offset' }

stepOffset : Input -> Offset -> Offset
stepOffset {delta,userInput} (x,y) =
    let (x',y') = userInput.scroll
    in ( x - x'*50
       , y + y'*50 )

stepPause : Input -> GameState -> GameState
stepPause {delta,userInput} gameState =
    gameState