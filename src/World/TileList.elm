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
    { tiles : List (ID, World.Tile.Model)
    , nextID : ID
    }

type alias ID =
    Int

default : Model
default =
    { tiles = [ (0, World.Tile.default) ]
    , nextID = 1
    }


-- UPDATE

type Action
    = NoOp
    | Insert World.Tile.TileType World.Position.Model
    | Draw World.Tile.TileType (List World.Position.Model)
    | Fill World.Tile.TileType (Int, Int)
    | Clear
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