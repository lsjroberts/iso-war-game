module World.World where

import List
import World.Tile
import World.Assets
import World.Position
import World.TileList
import Graphics.Collage


-- MODEL

type Action
    = Offset (Int, Int)

type alias Model =
    { tileList : World.TileList.Model }

default : Model
default =
    { tileList = World.TileList.default }


-- UPDATE

update : Action -> Model -> Model
update action model =
    case action of
        Offset (x,y) ->
            model
            --{ model | tileList <- List.map (World.Tile.update Offset) model.tiles }


-- VIEW

view : Model -> Graphics.Collage.Form
view model =
    model.tileList |> World.TileList.view
--                   |> Graphics.Collage.move -- ToDo