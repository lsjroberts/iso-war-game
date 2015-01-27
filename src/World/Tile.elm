module World.Tile where

import World.Assets
import World.Position
import Graphics.Collage
import Graphics.Element


-- MODEL

type Action
    = SetType TileType
    | Move World.Position.Model

type TileType = GrassTile | DirtTile | HillTopTile | HillNTile | HillNETile |
                HillETile | HillSETile | HillSTile | HillSWTile | HillWTile | HillNWTile

type alias Model =
    { tileType : TileType
    , pos : World.Position.Model
    }

default : Model
default =
    { tileType = GrassTile
    , pos = World.Position.default }

-- TODO: Refactor this to be a configurable value
zoom : Float
zoom = 0.5

tileSize : Int
tileSize = 131

init : TileType -> World.Position.Model -> Model
init tileType pos =
    { tileType = tileType
    , pos = pos
    }


-- UPDATE

update : Action -> Model -> Model
update action model =
    case action of
        SetType tileType ->
            { model | tileType <- tileType }

        Move pos ->
            { model | pos <- pos }


-- VIEW

view : Model -> Graphics.Collage.Form
view ({pos} as model) =
    let (x, y) = World.Position.translateToScreenCoords pos
    in model |> image
             |> Graphics.Collage.toForm
--             |> Graphics.Collage.move (x, y)

image : Model -> Graphics.Element.Element
image ({tileType} as model) =
    let w = floor <| (toFloat tileSize) * zoom
        h = floor <| (toFloat tileSize) * zoom
        path = tilePath tileType
    in Graphics.Element.image w h path

tilePath : TileType -> String
tilePath tileType =
    let tiles = World.Assets.tiles
    in
        case tileType of
            GrassTile   -> tiles.grass
            DirtTile    -> tiles.dirt
            HillTopTile -> tiles.hillTop
            HillNTile   -> tiles.hillN
            HillNETile  -> tiles.hillNE
            HillETile   -> tiles.hillE
            HillSETile  -> tiles.hillSE
            HillSTile   -> tiles.hillS
            HillSWTile  -> tiles.hillSW
            HillWTile   -> tiles.hillW
            HillNWTile  -> tiles.hillNW

-- |> Graphics.Input.hoverable (\on -> Signal.send tileActionChannel (if on then Hover tile else NoOp))
-- |> Graphics.Input.clickable (Signal.send tileActionChannel (Hover tile))