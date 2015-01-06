module World.Model where

import List (map,reverse,concatMap)

type TileType = GrassTile
              | DirtTile
              | HillTopTile
              | HillNTile
              | HillNETile
              | HillETile
              | HillSETile
              | HillSTile
              | HillSWTile
              | HillWTile
              | HillNWTile

type alias Position =
  { x:Int
  , y:Int
  , z:Int }

type alias Tile =
    { tileType:TileType
    , pos:Position }

type alias World =
    { tiles:List Tile }

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