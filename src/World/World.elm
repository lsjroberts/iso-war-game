module World.World where

import List
import Signal
import World.Tile
import LocalChannel
import World.Assets
import World.Position
import World.TileList
import Graphics.Collage


-- MODEL

type alias Model =
    { tileList : World.TileList.Model }

default : Model
default =
    { tileList =
        World.TileList.update
            (World.TileList.Fill World.Tile.BlankTile (8,8))
            World.TileList.default
    }


-- UPDATE

type Action
    = NoOp
    | Offset (Int, Int)
    | ModifyTileList World.TileList.Action

update : Action -> Model -> Model
update action model =
    case action of
        Offset (x,y) ->
            model
            --{ model | tileList <- List.map (World.Tile.update Offset) model.tiles }

        ModifyTileList tileListAction ->
            { model
                | tileList <- World.TileList.update tileListAction model.tileList
            }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    model.tileList |> viewTileList context
--                   |> Graphics.Collage.move -- ToDo

viewTileList : Context -> World.TileList.Model -> Graphics.Collage.Form
viewTileList context tileList =
    let context' =
            World.TileList.Context
                (LocalChannel.localize (ModifyTileList) context.actionChannel)
    in
        World.TileList.view context' tileList