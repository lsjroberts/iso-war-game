module Display where

import Graphics.Collage (Form,collage,group,move)
import Graphics.Element (Element,container,middle)

import Model
import Model (GameState)
import Game.Display (displayPlay,displayPause)
import Editor.Display (displayEditor)

display : (Int,Int) -> GameState -> Element
display (w,h) ({state} as gameState) =
    let forms = if | state == Model.Play -> displayPlay (w,h) gameState
                   | state == Model.Pause -> displayPause (w,h) gameState
                   | state == Model.Editor -> displayEditor (w,h) gameState
                   | otherwise -> []
    in container w h middle <| collage w h
        forms