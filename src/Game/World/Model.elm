module Game.World.Model where

import List (map,reverse,concatMap)
import Signal

type TileType = GrassTile | DirtTile | HillTopTile | HillNTile | HillNETile |
                HillETile | HillSETile | HillSTile | HillSWTile | HillWTile | HillNWTile

type alias Position =
    { x:Int
    , y:Int
    , z:Int }

type alias Tile =
    { tileType:TileType
    , pos:Position }

type alias World =
    { tiles:List Tile }

-- TODO: Refactor this to be a configurable value
zoom : Float
zoom = 1.0

tileSize : Int
tileSize = 131

--tileHover : Signal.Channel (Maybe Tile)
--tileHover =
--    Signal.channel Nothing

translatePos : Position -> (Float,Float)
translatePos ({x,y,z} as pos) =
    let w = (129 * zoom) / 2
        h = (-64 * zoom) / 2
        x' = toFloat x
        y' = toFloat y
        z' = toFloat z
    in ( x'*w + y'*w
       , y'*h - x'*h + z'*16)

filled : TileType -> Int -> Int -> Int -> World
filled tileType w h seed =
    let tiles = [1..w] |> concatMap (\i -> row tileType h seed i)
                       |> reverse
    in { tiles = tiles }

row : TileType -> Int -> Int -> Int -> List Tile
row tileType h seed i =
    [1..h] |> map (\j -> tile tileType seed i j 0)
           |> reverse

tile : TileType -> Int -> Int -> Int -> Int -> Tile
tile tileType seed i j k =
    { tileType = tileType
    , pos = { x = i
            , y = j
            , z = k } }

featureHill : List (Int,Int) -> Int -> World -> World
featureHill points seed world =
    world

featureRiver : List (Int,Int) -> Int -> World -> World
featureRiver points seed world =
    world