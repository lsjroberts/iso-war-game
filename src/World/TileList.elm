module World.TileList where

import List
import World.Tile
import World.Position
import Graphics.Collage

-- MODEL

type Action
    = Insert World.Tile.TileType World.Position.Model
    | Fill World.Tile.TileType (Int, Int)
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
            --model |> insertTile (World.Tile.init tileType pos)
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
                --model |> insertTiles tiles
                --area |> List.map (\pos -> update (Insert tileType pos) model)

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

--getNextTile : Model -> World.Tile.Model -> (ID, World.Tile.Model)
--getNextTile model tile =


--insertTile : World.Tile.Model -> Model -> Model
--insertTile tile model =
--    let tile' = getNextTile model tile
--        tiles' = model.tiles ++ [ tile' ]
--    in
--        { model |
--            tiles <- tiles',
--            nextID <- model.nextID + 1
--        }

--insertTiles : List World.Tile.Model -> Model -> Model
--insertTiles tiles model =
--    let tiles' = tiles |> List.map (\tile -> insertTile tile model)
--    in
--        { model | tiles <- tiles' }


-- VIEW

view : Model -> Graphics.Collage.Form
view model =
    let viewTile (tileID, tileModel) =
        World.Tile.view tileModel
    in
        model.tiles |> List.map viewTile
                    |> Graphics.Collage.group