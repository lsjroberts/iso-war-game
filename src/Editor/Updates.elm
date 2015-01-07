module Editor.Updates (stepEditor) where

import Input (Input)
import Model (GameState)
import Editor.Model (Editor)

stepEditor : Input -> GameState -> GameState
stepEditor input ({editor} as gameState) =
    { gameState | editor <- (
        case editor of
            Nothing -> Nothing
            Just editor ->
                Just (editor |> stepBrush input)
    ) }

stepBrush : Input -> Editor -> Editor
stepBrush input ({brush} as editor) =
    { editor | brush <- brush }