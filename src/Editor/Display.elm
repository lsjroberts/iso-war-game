module Editor.Display (displayEditor) where

import Graphics.Collage (Form,group,move)

import Model (GameState)

displayEditor : (Int,Int) -> GameState -> List Form
displayEditor dimensions ({offset} as gameState) =
    []