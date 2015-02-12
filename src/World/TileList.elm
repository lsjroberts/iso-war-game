module World.TileList where

import Debug (log)

import List
import Signal
import World.Tile
import LocalChannel
import World.Position
import Graphics.Collage

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

areaWithCost : Model -> Int -> World.Position.Model -> List CostedTile
areaWithCost ({tiles} as model) points fromPos =
    let (fromID, fromTile) = getTileAtPos model fromPos
    in
        tiles
            |> initCosts fromTile
            |> fillCosts (0, fromTile)
            |> List.filter (\(cost, tile) -> if cost <= points then True else False)

initCosts : World.Tile.Model -> List IndexedTile -> List CostedTile
initCosts start tiles =
    tiles
        |> List.map (\(id, tile) ->
            if tile == start
                then (0, tile)
                else (999, tile)
        )

fillCosts : CostedTile -> List CostedTile -> List CostedTile
fillCosts first tiles =
    fillNeighbourCosts first tiles
--        |> List.concatMap (\n -> fillNeighbourCosts n tiles)

fillNeighbourCosts : CostedTile -> List CostedTile -> List CostedTile
fillNeighbourCosts current tiles =
    let neighbours =
            tiles
                |> List.filter (isCostedNeighbour current)
    in
        neighbours
            |> List.map (setCost current)
--            |> List.map (\tile -> fillNeighbourCosts tile tiles)

setCost : CostedTile -> CostedTile -> CostedTile
setCost (currentCost, currentTile) (targetCost, targetTile) =
    let newCost = currentCost + 1
    in
        if newCost < targetCost
            then (newCost, targetTile)
            else (targetCost, targetTile)

isCostedNeighbour : CostedTile -> CostedTile -> Bool
isCostedNeighbour (aCost, aTile) (bCost, bTile) =
    isNeighbour aTile bTile

isNeighbour : World.Tile.Model -> World.Tile.Model -> Bool
isNeighbour tile check =
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
            let tiles' = positions |> List.map (\pos -> World.Tile.init tileType pos)
            in
                { model |
                    tiles <- List.indexedMap (,) tiles',
                    nextID <- model.nextID + List.length tiles'
                }

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


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({tiles} as model) =
    tiles |> List.map (viewTile context)
          |> Graphics.Collage.group

viewTile : Context -> (ID, World.Tile.Model) -> Graphics.Collage.Form
viewTile context (id, tile) =
    let context' =
            World.Tile.Context
                (LocalChannel.localize (ModifyTile id) context.actionChannel)
                (LocalChannel.localize (always (RemoveTile id)) context.actionChannel)
    in
        World.Tile.view context' tile