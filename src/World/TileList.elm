module World.TileList where

import List
import World.Tile
import World.Position
import Graphics.Collage

-- MODEL

type Action
    = Insert World.Tile.TileType World.Position.Model
    | Fill (Int, Int) World.Tile.TileType
    | Remove
    | Clear
    | Modify ID World.Tile.Action

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

update : Action -> Model -> Model
update action model =
    case action of
        Insert tileType pos ->
            model |> insertTile (World.Tile.init tileType pos)

        Fill dimensions tileType ->
            let area = World.Position.area dimensions
                tiles = area |> List.map (\pos -> World.Tile.init tileType pos)
            in
                model |> insertTiles tiles

        Remove ->
            { model | tiles <- List.drop 1 model.tiles }

        Clear ->
            { model | tiles <- [] }

        Modify id tileAction ->
            let updateTile (tileID, tileModel) =
                    if tileID == id
                        then (tileID, World.Tile.update tileAction tileModel)
                        else (tileID, tileModel)
            in
                { model | tiles <- model.tiles |> List.map updateTile }

insertTile : World.Tile.Model -> Model -> Model
insertTile tile model =
    let newTile = (model.nextID, tile)
        newTiles = model.tiles ++ [ newTile ]
    in
        { model |
            tiles <- newTiles,
            nextID <- model.nextID + 1
        }

insertTiles : List World.Tile.Model -> Model -> Model
insertTiles tiles model =
    tiles |> List.map (\tile -> insertTile tile model)

-- VIEW

view : Model -> Graphics.Collage.Form
view model =
    let viewTile (tileID, tileModel) =
        World.Tile.view tileModel
    in
        model.tiles |> List.map viewTile
                    |> Graphics.Collage.group