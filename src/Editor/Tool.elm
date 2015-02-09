module Editor.Tool where

import List
import World.Tile
import World.World
import LocalChannel
import Editor.Assets
import World.Position
import World.TileList
import Editor.Brushes
import Graphics.Input
import Graphics.Collage
import Graphics.Element

-- MODEL

type alias Model =
    { pos : World.Position.Model
    , brushType : Editor.Brushes.BrushType
    , isPainting : Bool
    }

default : Model
default =
    { pos = World.Position.init 0 0 0
    , brushType = Editor.Brushes.Elevation
    , isPainting = False
    }


-- UPDATE

type Action
    = NoOp
    | Place World.Position.Model
    | Move (Int, Int, Int)
    | SetBrushType Editor.Brushes.BrushType
    | Paint
    | Lift

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Place pos ->
            { model | pos <- pos }

        Move directions ->
            let pos' = model.pos |> World.Position.move directions
            in
                { model | pos <- pos' }

        SetBrushType brushType ->
            { model | brushType <- brushType }

        Paint ->
            { model | isPainting <- True }

        Lift ->
            { model | isPainting <- False }

paint : Model -> World.World.Model -> World.World.Model
paint model world =
    if model.isPainting
        then
            { world
                | tileList <- paintTileList model world.tileList
            }
        else
            world

paintTileList : Model -> World.TileList.Model -> World.TileList.Model
paintTileList model tileList =
    let tileType = Editor.Brushes.getTileTypeFromBrushType model.brushType
        updateTile (tileID, tileModel) =
            if tileModel.pos == model.pos
                then (tileID, World.Tile.update (World.Tile.SetType tileType) tileModel)
                else (tileID, tileModel)
    in
        { tileList
            | tiles <- tileList.tiles |> List.map updateTile
        }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    viewHint context model

viewHint : Context -> Model -> Graphics.Collage.Form
viewHint context model =
    let coords = World.Position.translateToScreen model.pos
    in
        model
            |> image
            |> Graphics.Input.clickable (LocalChannel.send context.actionChannel (if model.isPainting then Lift else Paint))
            |> Graphics.Collage.toForm
            |> Graphics.Collage.move coords

image : Model -> Graphics.Element.Element
image model =
    let w = floor <| (toFloat World.Tile.tileSize) * World.Tile.zoom
        h = floor <| (toFloat World.Tile.tileSize) * World.Tile.zoom
        path = Editor.Assets.getBrushSrc model.brushType
    in
        path |> Graphics.Element.image w h
             |> Graphics.Element.opacity 0.8