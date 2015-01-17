module Game.Updates (stepPlay,stepPause) where

import Input (Input)
import Model (GameState)
import Game.Model (Game,Offset,defaultGame)

stepPlay : Input -> GameState -> GameState
stepPlay ({delta,userInput} as input) ({game} as gameState) =
    { gameState | game <- (
        case game of
            Nothing -> Just defaultGame
            Just game -> Just (game |> stepGame input)
    ) }

stepGame : Input -> Game -> Game
stepGame input ({offset} as game) =
    let offset' = offset |> stepOffset input
    in { game | offset <- offset' }

stepOffset : Input -> Offset -> Offset
stepOffset {delta,userInput} (x,y) =
    let (x',y') = userInput.scroll
    in ( x - x'*50
       , y + y'*50 )

stepPause : Input -> GameState -> GameState
stepPause {delta,userInput} gameState =
    gameState