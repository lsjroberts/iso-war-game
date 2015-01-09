module Editor.Updates (stepEditor) where

import Input (Input)
import Model (GameState)
import Editor.Model (..)
import Game.World.Model (World,Tile)

stepEditor : Input -> GameState -> GameState
stepEditor input ({editor} as gameState) =
    { gameState | editor <- (
        case editor of
            Nothing -> Just defaultEditor
            Just editor ->
                Just (editor |> stepBrush input
                             |> stepWorld input )
    ) }

stepBrush : Input -> Editor -> Editor
stepBrush input ({brush} as editor) =
    { editor | brush <- (
        case brush of
            Nothing -> Nothing
            Just brush ->
                Just (brush |> stepIsPainting input)
    ) }

stepIsPainting : Input -> Brush -> Brush
stepIsPainting ({userInput} as input) ({isPainting} as brush) =
    { brush | isPainting <- if userInput.isMouseDown then True else False }

stepWorld : Input -> Editor -> Editor
stepWorld input ({world,brush} as editor) =
    { editor | world <- (
        case world of
            Nothing -> Nothing
            Just world ->
                Just (world |> stepWorldTiles brush)
    ) }

stepWorldTiles : Maybe Brush -> World -> World
stepWorldTiles ({tileType} as brush) ({tiles} as world) =
    { world | tiles <- (
        case brush of
            Nothing -> tiles
            Just brush ->
                if | tileType == Elevation -> paintElevation brush tiles
                   | otherwise -> paint brush tiles
    ) }

paintElevation : Brush -> List Tile -> List Tile
paintElevation brush tiles =
    tiles

paint : Brush -> List Tile -> List Tile
paint brush tiles =
    tiles


    --{ world | tiles <- (
    --    case brush of
    --        Nothing -> tiles
    --        Just brush ->
    --            tiles |> repaintTiles [ { tileType = tileType'
    --                                    , pos = pos' } ]
    --) }