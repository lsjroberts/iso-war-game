module Editor.Updates (stepEditor) where

import Input (Input)
import Model (GameState)
import Editor.Model (..)
import Game.World.Model (World,Tile)

stepEditor : Input -> GameState -> GameState
stepEditor input ({editor} as gameState) =
    { gameState | editor <-
        case editor of
            Nothing -> Just defaultEditor
            Just editor ->
                Just ( editor |> stepBrush input
                              |> stepWorld input )
    }

stepBrush : Input -> Editor -> Editor
stepBrush input ({brush} as editor) =
    { editor | brush <-
        case brush of
            Nothing -> Nothing
            Just brush ->
                Just ( brush |> stepBrushIsPainting input
                             |> stepBrushPosition input )
    }

stepBrushIsPainting : Input -> Brush -> Brush
stepBrushIsPainting ({userInput} as input) ({isPainting} as brush) =
    { brush | isPainting <-
        if | userInput.isMouseDown -> True
           | otherwise -> False }

stepBrushPosition : Input -> Brush -> Brush
stepBrushPosition ({userInput} as input) ({isPainting,pos} as brush) =
    { brush | pos <-
        if | isPainting ->
                Just { x = 0, y = 0, z = 0 } --userInput.mousePos
           | otherwise -> Nothing }

stepWorld : Input -> Editor -> Editor
stepWorld input ({world,brush} as editor) =
    { editor | world <-
        case world of
            Nothing -> Nothing
            Just world ->
                Just (world |> stepWorldTiles brush)
    }

stepWorldTiles : Maybe Brush -> World -> World
stepWorldTiles brush ({tiles} as world) =
    { world | tiles <-
        case brush of
            Nothing -> tiles
            Just brush -> tiles |> paintWorld brush
    }

paintWorld : Brush -> List Tile -> List Tile
paintWorld ({brushType} as brush) tiles =
    tiles |> if | brushType == Elevation -> paintElevation brush
                | otherwise -> paint brush

paintElevation : Brush -> List Tile -> List Tile
paintElevation brush tiles =
    tiles

paint : Brush -> List Tile -> List Tile
paint brush tiles =
    tiles