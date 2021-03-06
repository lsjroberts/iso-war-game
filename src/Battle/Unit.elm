module Battle.Unit where

import Debug (log)

import World.Tile
import LocalChannel
import Battle.Assets
import World.Position
import Graphics.Input
import Graphics.Collage
import Graphics.Element


-- MODEL

type UnitType
    = Infantry
    | Tank

type alias Model =
    { unitType : UnitType
    , pos : World.Position.Model
    , selected : Bool
    , movementPoints : Points
    , actionPoints : Points
    }

type alias Points =
    Int

unitWidth = 131
unitHeight = 131
zoom = 0.5

init : UnitType -> World.Position.Model -> Model
init unitType pos =
    { unitType = unitType
    , pos = pos
    , selected = False
    , movementPoints = movementPointsForUnitType unitType
    , actionPoints = actionPointsForUnitType unitType
    }

infantry : World.Position.Model -> Model
infantry pos =
    init Infantry pos

tank : World.Position.Model -> Model
tank pos =
    init Tank pos

movementPointsForUnitType : UnitType -> Points
movementPointsForUnitType unitType =
    case unitType of
        Infantry -> 3
        Tank -> 6

actionPointsForUnitType : UnitType -> Points
actionPointsForUnitType unitType =
    case unitType of
        Infantry -> 2
        Tank -> 2

tileTypeCostForUnitType : UnitType -> World.Tile.TileType -> Points
tileTypeCostForUnitType unitType tileType =
    case unitType of
        Infantry ->
            tileTypeCostForInfantry tileType
        Tank ->
            tileTypeCostForTank tileType

tileTypeCostForInfantry : World.Tile.TileType -> Points
tileTypeCostForInfantry tileType =
    case tileType of
        World.Tile.GrassTile -> 1
        World.Tile.DirtTile  -> 1

tileTypeCostForTank : World.Tile.TileType -> Points
tileTypeCostForTank tileType =
    case tileType of
        World.Tile.GrassTile -> 1
        World.Tile.DirtTile  -> 1

hasPoints : Model -> Bool
hasPoints model =
    if model.actionPoints > 0 || model.movementPoints > 0
        then True
        else False

isSelected : Model -> Bool
isSelected model =
    model.selected


-- UPDATE

type Action
    = NoOp
    | ToggleSelect
    | Select
    | Unselect
    | Place World.Position.Model

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ToggleSelect ->
            { model | selected <- not model.selected }

        Select ->
            { model | selected <- True }

        Unselect ->
            { model | selected <- False }

        Place pos ->
            { model | pos <- pos }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({pos} as model) =
    let (x, y) = World.Position.translateToScreen pos
    in
        model
            |> image
            |> Graphics.Collage.toForm
            |> Graphics.Collage.move (x,y)

image : Model -> Graphics.Element.Element
image ({unitType, selected} as model) =
    let w = floor <| (toFloat unitWidth) * zoom
        h = floor <| (toFloat unitHeight) * zoom
        path = unitPath unitType
    in
        path |> Graphics.Element.image w h
             --|> if selected
             --       then Graphics.Element.opacity 1
             --       else Graphics.Element.opacity 0.5

unitPath : UnitType -> String
unitPath unitType =
    let units = Battle.Assets.units
    in
        case unitType of
            Infantry -> units.infantry
            Tank     -> units.tank