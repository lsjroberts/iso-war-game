module Editor.Display (displayEditor) where

import Color (rgb)
import Graphics.Collage (Form,group,move,filled,polygon,alpha,toForm)
import Graphics.Element (image)

import Model (GameState)
import Editor.Assets (getBrushSrc)
import Editor.Model (Editor,Brush)
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
            ( displayBrush dimensions editor ) ++
            ( displayTools dimensions editor )

displayBrush : (Int,Int) -> Editor -> List Form
displayBrush dimensions ({brush} as editor) =
    case brush of
        Nothing -> []
        Just brush ->
            [ displayBrushMouseHint dimensions brush ]

displayBrushMouseHint : (Int,Int) -> Brush -> Form
displayBrushMouseHint dimensions brush =
    image 131 131 (getBrushSrc brush.brushType) |> toForm
                                                |> move (0,0)
                                                |> alpha 0.5

displayTools : (Int,Int) -> Editor -> List Form
displayTools dimensions ({brush} as editor) =
    []