module World.Tile where

import Debug (log)

import Signal
import LocalChannel
import World.Assets
import World.Position
import Graphics.Input
import Graphics.Collage
import Graphics.Element


-- MODEL

type TileType = BlankTile | GrassTile | DirtTile | HillTopTile | HillNTile | HillNETile |
                HillETile | HillSETile | HillSTile | HillSWTile | HillWTile | HillNWTile

type alias Model =
    { tileType : TileType
    , pos : World.Position.Model
    }

init : TileType -> World.Position.Model -> Model
init tileType pos =
    { tileType = tileType
    , pos = pos
    }

default : Model
default =
    init BlankTile World.Position.default

-- TODO: Refactor this to be a configurable value
zoom : Float
zoom = 0.5

tileSize : Int
tileSize = 131


-- UPDATE

type Action
    = NoOp
    | Click
    | SetType TileType
    | Move World.Position.Model

update : Action -> Model -> Model
update action model =
    case action of
        SetType tileType ->
            { model | tileType <- (log "SetType" tileType) }
        Move pos ->
            { model | pos <- pos }
        NoOp ->
            model


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    , removeChannel : LocalChannel.LocalChannel ()
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({pos} as model) =
    let (x, y) = World.Position.translateToScreenCoords pos
    in
        model
            |> image
            |> Graphics.Input.clickable (LocalChannel.send context.actionChannel (SetType GrassTile))
--          |> Graphics.Input.hoverable (\on -> LocalChannel.send context.actionChannel (if on then (SetType BlankTile) else (SetType DirtTile)))
            |> Graphics.Collage.toForm
            |> Graphics.Collage.move (x,y)
--          |> Graphics.Input.hoverable (\on -> Signal.send channel (if on then Hover tile else NoOp))
--          |> Graphics.Input.clickable (Signal.send channel (Click tile))

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
            BlankTile   -> tiles.blank
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