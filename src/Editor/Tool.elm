module Editor.Tool where

import List
import World.Tile
import World.World
import World.TileList
import LocalChannel
import World.Position
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

        --Paint point ->
        --    let paint (world) =
        --        { world | tileList <- World.TileList.update Insert GrassTile {x=0,y=1,z=0} }
        --    in
        --        { model | world <- paint model.world }

        --PaintArea (topLeft, bottomRight) ->
        --    model

        --Clear ->
        --    { model | world <- World.World.update Clear model.world }

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
    let updateTile (tileID, tileModel) =
            if tileModel.pos == model.pos
                then (tileID, World.Tile.update World.Tile.SetType World.Tile.GrassTile)
                else (tileID, tileModel)
    in
        { model
            | tiles <- model.tiles |> List.map updateTile
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
    let (x, y) = World.Position.translateToScreenCoords model.pos
    in
        model
            |> image
            |> Graphics.Input.clickable (LocalChannel.send context.actionChannel Paint)
            |> Graphics.Collage.toForm

image : Model -> Graphics.Element.Element
image model =
    let w = 100 --floor <| (toFloat tileSize) * zoom
        h = 100 --floor <| (toFloat tileSize) * zoom
        path = ""
    in Graphics.Element.image w h path