module Editor.Display (displayEditor) where

import Color (rgb)
import Graphics.Collage (Form,group,move,filled,polygon)

import Model (GameState)
import Editor.Model (Editor)
import Game.World.Display (displayWorld)

displayEditor : (Int,Int) -> GameState -> List Form
displayEditor dimensions ({editor} as gameState) =
    case editor of
        Nothing -> []
        Just editor -> editor |> displayEditorView dimensions

displayEditorView : (Int,Int) -> Editor -> List Form
displayEditorView dimensions ({world} as editor) =
    case world of
        Nothing -> []
        Just world ->
            [ displayWorld dimensions world |> group ] ++
            ( displayTools dimensions editor )

displayTools : (Int,Int) -> Editor -> List Form
displayTools dimensions ({brush} as editor) =
    []