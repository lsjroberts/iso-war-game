module Game.World.Display (displayWorld) where

import List (map)
import Color (rgb)
import Graphics.Collage (Form,move,toForm)
import Graphics.Element (Element,image)

import Game.World.Model (Position,Tile,TileType,World)
import Game.World.Assets (getTileImageSrc)

-- TODO: Refactor this out to a configurable value
zoom : Float
zoom = 1

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
    in image (floor <| 131 * zoom) (floor <| 131 * zoom) src |> toForm
                                                             |> move (x,y)

translatePos : Position -> (Float,Float)
translatePos ({x,y,z} as pos) =
    let w = (129 * zoom) / 2
        h = (-64 * zoom) / 2
        x' = toFloat x
        y' = toFloat y
        z' = toFloat z
    in ( x'*w + y'*w
       , y'*h - x'*h + z'*16)

displayWorld : (Int,Int) -> World -> List Form
displayWorld (w,h) world =
    world |> sortTiles
          |> displayTiles