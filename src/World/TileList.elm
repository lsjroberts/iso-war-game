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

type alias ID = Int

default : Model
default =
    { tiles = [ (0, World.Tile.default) ]
    , nextID = 1
    }


-- UPDATE

type Action
    = NoOp
    | Insert World.Tile.TileType World.Position.Model
    | Fill World.Tile.TileType (Int, Int)
    | Clear
    | Remove ID
    | Modify ID World.Tile.Action

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

        Modify id tileAction ->
            let updateTile (tileID, tileModel) =
                    if tileID == (log "modify id" id)
                        then (tileID, World.Tile.update (log "tileAction" tileAction) tileModel)
                        else (tileID, tileModel)
            in
                { model |
                    tiles <- model.tiles |> List.map updateTile
                }

        Remove id ->
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
                (LocalChannel.localize (Modify id) context.actionChannel)
                (LocalChannel.localize (always (Remove id)) context.actionChannel)
    in
        World.Tile.view context' tile


-- SIGNALS

--actionChannel : Signal.Channel Action
--actionChannel =
--    Signal.channel NoOp