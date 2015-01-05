module Display where

import Graphics.Collage (Form,collage,group,move)
import Graphics.Element (Element,container,middle)

import World.Display
import Model
import Model (GameState)

display : (Int,Int) -> GameState -> Element
display (w,h) ({state} as gameState) =
    let forms = if | state == Model.Play  -> displayPlay (w,h) gameState
                   | state == Model.Pause -> displayPause (w,h) gameState
                   | otherwise -> []
    in container w h middle <| collage w h
        forms
        --[ filled (rgb 255 0 0) (rect 20 20) ]

displayPlay : (Int,Int) -> GameState -> List Form
displayPlay dimensions ({offset} as gameState) =
    let displayed =
            [ World.Display.display dimensions gameState |> group
                                                         |> move offset ]
    in displayed

displayPause : (Int,Int) -> GameState -> List Form
displayPause dimensions gameState =
    []