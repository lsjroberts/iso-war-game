module Battle.Player where

import Debug (log)

import World.World
import Battle.Army
import LocalChannel
import Battle.Assets
import Battle.Cursor
import World.Position
import Graphics.Collage
import Graphics.Element
import Battle.CursorList


-- MODEL

type alias Model =
    { name : String
    , army : Battle.Army.Model
    , cursor : Battle.Cursor.Model
    , movementCursors : Battle.CursorList.Model
    , attackCursors : Battle.CursorList.Model
    }

demoHuman : Model
demoHuman =
    { name = "Human"
    , army = Battle.Army.demo
    , cursor = Battle.Cursor.standard
    , movementCursors = Battle.CursorList.default
    , attackCursors = Battle.CursorList.default
    }


-- UPDATE

type Action
    = NoOp
    | SelectNextUnit
    | ToggleSelectedUnitCursorMode
    | EnactCursor
    | ModifyArmy Battle.Army.Action
    | ModifyCursor Battle.Cursor.Action
    | ModifyMovementCursors Battle.CursorList.Action
    | ModifyAttackCursors Battle.CursorList.Action

update : Action -> World.World.Model -> Model -> Model
update action world model =
    case action of
        NoOp ->
            model

        SelectNextUnit ->
            let (unitID, unitModel) =
                    Battle.Army.cycleUnitsWithPoints model.army
                army =
                    Battle.Army.update (Battle.Army.SelectUnit unitID) model.army
                cursor =
                    Battle.Cursor.update (Battle.Cursor.Place unitModel.pos) model.cursor
                movementCursors =
                    Battle.CursorList.update (Battle.CursorList.AreaWithCost Battle.Cursor.Movement 2 world unitModel.pos) model.movementCursors
                attackCursors =
                    Battle.CursorList.update Battle.CursorList.Clear model.attackCursors
            in
                { model
                    | army <- army
                    , cursor <- cursor
                    , movementCursors <- movementCursors
                    , attackCursors <- attackCursors
                }

        ToggleSelectedUnitCursorMode ->
            let (unitID, unitModel) =
                    Battle.Army.getSelectedUnit model.army
                movementCursors =
                    Battle.CursorList.update Battle.CursorList.Clear model.movementCursors
                attackCursors =
                    Battle.CursorList.update (Battle.CursorList.AreaCircle Battle.Cursor.Attack 1 unitModel.pos) model.attackCursors
            in
                { model
                    | movementCursors <- movementCursors
                    , attackCursors <- attackCursors
                }

        EnactCursor ->
            let (unitID, unitModel) =
                    Battle.Army.getSelectedUnit model.army
                cursor =
                    model.cursor
            in
                model
                    |> update (ModifyArmy (Battle.Army.MoveUnit unitID cursor.pos)) world
                    |> update SelectNextUnit world

        ModifyArmy armyAction ->
            { model | army <- model.army |> Battle.Army.update armyAction }

        ModifyCursor cursorAction ->
            --model
            { model | cursor <- model.cursor |> Battle.Cursor.update cursorAction }
            --let cursor = model.cursor |> Battle.Cursor.update (log "cursorAction" cursorAction)
            --    army =
            --        if cursorAction == Battle.Cursor.SelectUnit
            --            then Battle.Army.update (Battle.Army.SelectUnitAtPosition cursor.pos) model.army
            --            else model.army
            --in
            --    { model
            --        | cursor <- cursor
            --        , army <- army
            --    }

        ModifyMovementCursors cursorsAction ->
            { model | movementCursors <- model.movementCursors |> Battle.CursorList.update cursorsAction }

        ModifyAttackCursors cursorsAction ->
            { model | attackCursors <- model.attackCursors |> Battle.CursorList.update cursorsAction }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let forms =
            [ viewCursor context model.cursor
            , viewMovementCursors context model.movementCursors
            , viewAttackCursors context model.attackCursors
            , viewArmy context model.army
            ]
    in
        forms |> Graphics.Collage.group

viewArmy : Context -> Battle.Army.Model -> Graphics.Collage.Form
viewArmy context army =
    let context' =
            Battle.Army.Context
                (LocalChannel.localize (ModifyArmy) context.actionChannel)
    in
        Battle.Army.view context' army

viewCursor : Context -> Battle.Cursor.Model -> Graphics.Collage.Form
viewCursor context cursor =
    let context' =
            Battle.Cursor.Context
                (LocalChannel.localize (ModifyCursor) context.actionChannel)
    in
        Battle.Cursor.view context' cursor

viewMovementCursors : Context -> Battle.CursorList.Model -> Graphics.Collage.Form
viewMovementCursors context cursors =
    let context' =
            Battle.CursorList.Context
                (LocalChannel.localize (ModifyMovementCursors) context.actionChannel)
    in
        Battle.CursorList.view context' cursors

viewAttackCursors : Context -> Battle.CursorList.Model -> Graphics.Collage.Form
viewAttackCursors context cursors =
    let context' =
            Battle.CursorList.Context
                (LocalChannel.localize (ModifyAttackCursors) context.actionChannel)
    in
        Battle.CursorList.view context' cursors