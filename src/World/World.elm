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
    { tileList : World.TileList.Model
    , offset : (Float, Float)
    }

default : Model
default =
    { tileList =
        World.TileList.update
            (World.TileList.Fill World.Tile.BlankTile (8,8))
            World.TileList.default
    , offset = (0, 0)
    }

demo : Model
demo =
    { tileList =
        World.TileList.default
            |> World.TileList.update (World.TileList.Fill World.Tile.GrassTile (16,16))
            |> World.TileList.update (World.TileList.DrawArea World.Tile.RiverVTile
                    (World.Position.init 3 0 0, World.Position.init 3 4 0)
            )
            |> World.TileList.update (World.TileList.Draw World.Tile.RiverTurnUpRightTile
                    [World.Position.init 3 5 0]
            )
            |> World.TileList.update (World.TileList.DrawArea World.Tile.RiverHTile
                    (World.Position.init 4 5 0, World.Position.init 8 5 0)
            )
            |> World.TileList.update (World.TileList.Draw World.Tile.RiverTurnLeftUpTile
                    [World.Position.init 9 5 0]
            )
            |> World.TileList.update (World.TileList.DrawArea World.Tile.RiverVTile
                    (World.Position.init 9 6 0, World.Position.init 9 16 0)
            )
            |> World.TileList.update (World.TileList.DrawArea World.Tile.HillTopTile
                    (World.Position.init 2 0 0, World.Position.init 2 4 0)
            )
    , offset = (0, 0)
    }

--areaWithCost : Model -> Int -> World.Position.Model -> List (Int, World.Position.Model)
--areaWithCost world points centre =
--    World.TileList.areaWithCost world.tileList points centre
--        |> List.map (\(id, tile) -> (id, tile.pos))

getDistanceGrid : Model -> Int -> World.Position.Model -> List (Int, World.Position.Model)
getDistanceGrid world points start =
    World.TileList.getDistanceGridFromPos start world.tileList
        |> List.map (\(cost, tile) -> (cost, tile.pos))


-- UPDATE

type Action
    = NoOp
    | ModifyTileList World.TileList.Action

update : Action -> Model -> Model
update action model =
    case action of
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
    model.tileList
        |> viewTileList context
        |> Graphics.Collage.move model.offset

viewTileList : Context -> World.TileList.Model -> Graphics.Collage.Form
viewTileList context tileList =
    let context' =
            World.TileList.Context
                (LocalChannel.localize (ModifyTileList) context.actionChannel)
    in
        World.TileList.view context' tileList