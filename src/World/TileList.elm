module World.TileList where

import Debug (log)

import List
import Signal
import LocalChannel
import Graphics.Collage

import Helpers

import World.Tile
import World.Position

-- MODEL

type alias Model =
    { tiles : List IndexedTile
    , nextID : ID
    }

type alias ID = Int
type alias Cost = Int
type alias IndexedTile = (ID, World.Tile.Model)
type alias CostedTile = (Cost, World.Tile.Model)

default : Model
default =
    { tiles = [ (0, World.Tile.default) ]
    , nextID = 1
    }

getTileAtPos : Model -> World.Position.Model -> IndexedTile
getTileAtPos model pos =
    model.tiles
        |> List.filter (\(id, tile) -> if tile.pos == pos then True else False)
        |> List.head

getDistanceGridFromPos : World.Position.Model -> Model -> List CostedTile
getDistanceGridFromPos pos model =
    let start = getTileAtPos model pos
    in
        getDistanceGrid start model

getDistanceGrid : IndexedTile -> Model -> List CostedTile
getDistanceGrid start model =
    let (startID, startTile) = start
    in
        visitFrontier model 0 (getNeighbours start model.tiles) [(0, startTile)]

visitFrontier : Model -> Int -> List IndexedTile -> List CostedTile -> List CostedTile
visitFrontier model cost frontier visited =
    if List.length frontier > 0
        then frontier |> List.concatMap (visitFrontierTile model cost visited)
        else []

visitFrontierTile : Model -> Int -> List CostedTile -> IndexedTile -> List CostedTile
visitFrontierTile model cost visited next =
    let grid = model.tiles |> List.filter (\t -> not (List.member t visited))
        nextCost = cost + 1
    in
        if not (List.member next visited)
            then
                if cost < 3
                    then
                        next :: (visitFrontier model nextCost (getNeighbours next grid) (next :: visited))
                    else []
            else []

--getShortestPath : IndexedTile -> IndexedTile -> List IndexedTile -> List CostedTile
--getShortestPath start finish grid =
--    let helper frontier visited =
--            if | List.length frontier > 0 ->
--                    helper (getNeighbours (List.head frontier) (List.filter grid)
--               | otherwise ->
--                    visited
--    in
--        helper (getNeighbours start grid) []

--getShortestPathNeighbours : List IndexedTile -> List IndexedTile -> List IndexedTile
--getShortestPathNeighbours neighbours visited =
--    let current = List.head
--    if | List.length frontier == 0 ->
--                    visited
--               | otherwise ->
--                    helper (getNeighbours (List.head frontier) (List.filter grid)

getNeighbours : IndexedTile -> List IndexedTile -> List IndexedTile
getNeighbours tile grid =
    grid |> List.filter (isNeighbour tile)




--areaWithCost : Model -> Int -> World.Position.Model -> List CostedTile
--areaWithCost ({tiles} as model) points fromPos =
--    let (fromID, fromTile) = getTileAtPos model fromPos
--    in
--        tiles
--            |> initCosts fromTile
--            |> fillCosts (0, fromTile)
--            |> List.filter (\(cost, tile) -> if cost <= points then True else False)

--initCosts : World.Tile.Model -> List IndexedTile -> List CostedTile
--initCosts start tiles =
--    tiles
--        |> List.map (\(id, tile) ->
--            if tile == start
--                then (0, tile)
--                else (999, tile)
--        )

--fillCosts : CostedTile -> List CostedTile -> List CostedTile
--fillCosts first tiles =
--    fillNeighbourCosts first tiles
----        |> List.concatMap (\n -> fillNeighbourCosts n tiles)

--fillNeighbourCosts : CostedTile -> List CostedTile -> List CostedTile
--fillNeighbourCosts current tiles =
--    let neighbours =
--            tiles
--                |> List.filter (isCostedNeighbour current)
--    in
--        neighbours
--            |> List.map (setCost current)
----            |> List.map (\tile -> fillNeighbourCosts tile tiles)

--setCost : CostedTile -> CostedTile -> CostedTile
--setCost (currentCost, currentTile) (targetCost, targetTile) =
--    let newCost = currentCost + 1
--    in
--        if newCost < targetCost
--            then (newCost, targetTile)
--            else (targetCost, targetTile)

--isCostedNeighbour : CostedTile -> CostedTile -> Bool
--isCostedNeighbour (aCost, aTile) (bCost, bTile) =
--    isNeighbour aTile bTile

isNeighbour : IndexedTile -> IndexedTile -> Bool
isNeighbour (tileID, tile) (checkID, check) =
    let tilePos = tile.pos
        checkPos = check.pos
        diffX = tilePos.x - checkPos.x |> abs
        diffY = tilePos.y - checkPos.y |> abs
    in
        if ((diffX == 0 && diffY == 1) || (diffX == 1 && diffY == 0))
            then True
            else False


-- UPDATE

type Action
    = NoOp
    | Insert World.Tile.TileType World.Position.Model
    | Draw World.Tile.TileType (List World.Position.Model)
    | DrawArea World.Tile.TileType (World.Position.Model, World.Position.Model)
    | Fill World.Tile.TileType (Int, Int)
    | Clear
    | Offset (Float, Float)
    | ModifyTile ID World.Tile.Action
    | RemoveTile ID

update : Action -> Model -> Model
update action model =
    case action of
        Insert tileType pos ->
            let tile' = (model.nextID, World.Tile.init tileType pos)
                tiles' = model.tiles ++ [ tile' ]
            in
                { model |
                    tiles <- tiles',
                    nextID <- model.nextID + 1
                }

        Draw tileType positions ->
            model |> draw tileType positions

        DrawArea tileType (start, end) ->
            update (Draw tileType (World.Position.bounds (start, end))) model

        Fill tileType dimensions ->
            let area = World.Position.area dimensions
                tiles' = area |> List.map (\pos -> World.Tile.init tileType pos)
            in
                { model |
                    tiles <- List.indexedMap (,) tiles',
                    nextID <- List.length tiles'
                }

        Clear ->
            { model | tiles <- [] }

        ModifyTile id tileAction ->
            let updateTile (tileID, tileModel) =
                    if tileID == id
                        then (tileID, World.Tile.update tileAction tileModel)
                        else (tileID, tileModel)
            in
                { model |
                    tiles <- model.tiles |> List.map updateTile
                }

        RemoveTile id ->
            { model |
                tiles <- List.filter (\(tileID, _) -> tileID /= id) model.tiles
            }

draw : World.Tile.TileType -> List World.Position.Model -> Model -> Model
draw tileType positions world =
    let tiles' =
            world.tiles
                |> List.partition (\(id, tile) -> if List.member tile.pos positions then True else False)
                |> drawPartition tileType
    in
        { world |
            tiles <- tiles'
        }

drawPartition : World.Tile.TileType -> (List IndexedTile, List IndexedTile) -> List IndexedTile
drawPartition tileType (drawTiles, ignoreTiles) =
    (drawTiles |> List.map (drawTile tileType))
    ++
    (ignoreTiles |> List.filter (\t -> if List.member t drawTiles then False else True))

drawTile : World.Tile.TileType -> IndexedTile -> IndexedTile
drawTile tileType (tileID, tileModel) =
    ( tileID
    , tileModel |> World.Tile.update (World.Tile.SetType tileType)
    )

-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({tiles} as model) =
    tiles
        |> List.sortWith tilesViewSorter
        |> List.map (viewTile context)
        |> Graphics.Collage.group

tilesViewSorter : IndexedTile -> IndexedTile -> Order
tilesViewSorter (aID, aModel) (bID, bModel) =
    let aPos = aModel.pos
        bPos = bModel.pos
    in
        if aPos.y > bPos.y
            then LT
            else if aPos.y < bPos.y
                then GT
                else if aPos.x < bPos.x
                    then LT
                    else GT

viewTile : Context -> (ID, World.Tile.Model) -> Graphics.Collage.Form
viewTile context (id, tile) =
    let context' =
            World.Tile.Context
                (LocalChannel.localize (ModifyTile id) context.actionChannel)
                (LocalChannel.localize (always (RemoveTile id)) context.actionChannel)
    in
        World.Tile.view context' tile