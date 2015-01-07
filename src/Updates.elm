module Updates (step) where

import Input (Input)
import Model
import Model (GameState)
import Game.Updates (stepPlay,stepPause)
import Editor.Updates (stepEditor)

step : Input -> GameState -> GameState
step input ({state} as gameState) =
    if | state == Model.Play   -> gameState |> stepPlay input
       | state == Model.Pause  -> gameState |> stepPause input
       | state == Model.Editor -> gameState |> stepEditor input
       | otherwise -> gameState