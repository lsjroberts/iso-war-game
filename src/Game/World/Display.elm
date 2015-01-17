module Game.World.Display (displayWorld) where

import List (map)
import Color (rgb)
import Signal
import Graphics.Input (hoverable)
import Graphics.Collage (Form,move,toForm)
import Graphics.Element (Element,image)

import Game.World.Model (Position,Tile,TileType,World,tileSize,tileHover,zoom,translatePos)
import Game.World.Assets (getTileImageSrc)

sortTiles : World -> World
sortTiles world =
    world
    --world |> sort

displayTiles : World -> List Form
displayTiles ({tiles} as world) =
    tiles |> map (\t -> displayTile t)

displayTile : Tile -> Form
displayTile ({tileType,pos} as tile) =
    let (x,y) = translatePos pos
    in tile |> getTileImage (getTileImageSrc tileType)
            --|> hoverable (Signal.send tileHover (Just tile))
            |> toForm
            |> move (x,y)

getTileImage : String -> Tile -> Element
getTileImage src ({pos} as tile) =
    let w = (floor <| (toFloat tileSize) * zoom)
        h = (floor <| (toFloat tileSize) * zoom)
    in image w h src


displayWorld : (Int,Int) -> World -> List Form
displayWorld (w,h) world =
    world |> sortTiles
          |> displayTiles