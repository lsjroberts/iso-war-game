module Editor.Display (displayEditor) where

import Color (rgb)
import Graphics.Collage (Form,group,move,filled,polygon,alpha,toForm)
import Graphics.Element (image)

import Model (GameState)
import Editor.Assets (getBrushSrc)
import Editor.Model (Editor,Brush,BrushType)
import Game.World.Model (Position,tileSize,translatePos)
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
            [ displayBrushPointer dimensions brush ]

displayBrushPointer : (Int,Int) -> Brush -> Form
displayBrushPointer dimensions ({brushType,pos} as brush) =
    let (x,y) = translatePos pos
    in image tileSize tileSize (getBrushSrc brushType) |> toForm
                                                       |> move (x,y)
                                                       |> alpha 0.5

displayTools : (Int,Int) -> Editor -> List Form
displayTools dimensions ({brush} as editor) =
    []