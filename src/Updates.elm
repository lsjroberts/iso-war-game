module Updates (step) where

import Input (Input)
import Model
import Model (GameState,State,Offset)

step : Input -> GameState -> GameState
step input ({state} as gameState) =
    if | state == Model.Play  -> gameState |> stepPlay input
       | state == Model.Pause -> gameState |> stepPause input
       | otherwise -> gameState

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