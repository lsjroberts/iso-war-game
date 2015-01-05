module World.Display (display) where

import List (map)
import Color (rgb)
import Graphics.Collage (Form,move,toForm)
import Graphics.Element (Element,image)

import Model (GameState)
import World.Model (Position,Tile,TileType,World)
import World.Assets (getTileImageSrc)

sortTiles : World -> World
sortTiles world =
    world
    --world |> sort

displayTiles : World -> List Form
displayTiles ({tiles} as world) =
    tiles |> map (\t -> displayTile t)

displayTile : Tile -> Form
displayTile ({tileType} as tile) =
    tile |> getTileImage (getTileImageSrc tileType)

getTileImage : String -> Tile -> Form
getTileImage src ({pos} as tile) =
    let (x,y) = translatePos pos
    in image 131 131 src |> toForm
                         |> move (x,y)

translatePos : Position -> (Float,Float)
translatePos ({x,y,z} as pos) =
    let w = 129/2
        h = -64/2
        x' = toFloat x
        y' = toFloat y
        z' = toFloat z
    in ( x'*w + y'*w
       , y'*h - x'*h + z'*16)

display : (Int,Int) -> GameState -> List Form
display (w,h) ({world} as gameState) =
    world |> sortTiles
          |> displayTiles