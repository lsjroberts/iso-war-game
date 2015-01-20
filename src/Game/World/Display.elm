module Game.World.Display (displayWorld) where

import List
import Signal
import Html.Events
import Graphics.Input
import Graphics.Element
import Graphics.Collage

import Game.World.Model (..)
import Game.World.Input (..)
import Game.World.Assets (getTileImageSrc)

displayWorld : (Int,Int) -> World -> List Graphics.Collage.Form
displayWorld (w,h) world =
    world |> sortTiles
          |> displayTiles

sortTiles : World -> World
sortTiles world =
    world

displayTiles : World -> List Graphics.Collage.Form
displayTiles ({tiles} as world) =
    tiles |> List.map (\t -> displayTile t)

displayTile : Tile -> Graphics.Collage.Form
displayTile ({tileType,pos} as tile) =
    let (x,y) = translatePos pos
    in tile |> getTileImage (getTileImageSrc tileType)
            |> Graphics.Input.hoverable (\on -> Signal.send tileInput (if on then Hover tile else NoOp))
            |> Graphics.Input.clickable (Signal.send tileInput (Hover tile))
            |> Graphics.Collage.toForm
            |> Graphics.Collage.move (x,y)

getTileImage : String -> Tile -> Graphics.Element.Element
getTileImage src ({pos} as tile) =
    let w = (floor <| (toFloat tileSize) * zoom)
        h = (floor <| (toFloat tileSize) * zoom)
    in Graphics.Element.image w h src