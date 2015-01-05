module World.Model where

import List (map,reverse,concatMap)

type TileType = GrassTile | DirtTile

type alias Position =
  { x:Int
  , y:Int
  , z:Int }

type alias Tile =
    { tileType:TileType
    , pos:Position }

type alias World =
    { tiles:List Tile }

generate : Int -> Int -> Int -> World
generate w h seed =
    let tiles = [1..w] |> concatMap (\i -> generateRow h seed i)
                       |> reverse
    in { tiles = tiles }

generateRow : Int -> Int -> Int -> List Tile
generateRow h seed i =
    [1..h] |> map (\j -> generateTile seed i j)
           |> reverse

generateTile : Int -> Int -> Int -> Tile
generateTile seed i j =
    { tileType = GrassTile
    , pos = { x = i
            , y = j
            , z = 0 } }

featureHill : (Int,Int) -> Int -> Int -> List Tile -> List Tile
featureHill (x,y) size seed tiles =
    tiles

featureRiver : (Int,Int) -> (Int,Int) -> Int -> Int -> List Tile -> List Tile
featureRiver (x,y) (x',y') width seed tiles =
    tiles

demo : World
demo =
    let tiles = [ { tileType = GrassTile, pos = {x=2,y=0,z=0} }
                , { tileType = DirtTile, pos = {x=1,y=0,z=0} }
                , { tileType = GrassTile, pos = {x=0,y=0,z=0} }
                , { tileType = DirtTile, pos = {x=-1,y=0,z=0} }
                , { tileType = GrassTile, pos = {x=-2,y=0,z=0} }
                , { tileType = GrassTile, pos = {x=-3,y=0,z=0} }
                , { tileType = DirtTile,  pos = {x=2,y=1,z=0} }
                , { tileType = DirtTile,  pos = {x=1,y=1,z=0} }
                , { tileType = GrassTile,  pos = {x=0,y=1,z=1} }
                , { tileType = GrassTile,  pos = {x=-1,y=1,z=0} }
                , { tileType = DirtTile,  pos = {x=-2,y=1,z=0} }
                , { tileType = DirtTile,  pos = {x=-3,y=1,z=0} } ]
    in { tiles = tiles }