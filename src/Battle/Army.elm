module Battle.Army where

import Debug (log)

import List
import Signal
import Battle.Unit
import LocalChannel
import World.Position
import Graphics.Collage

-- MODEL

type alias Model =
    { units : List (ID, Battle.Unit.Model)
    , nextUnitID : ID
    , selectedUnitID : ID
    }

type alias ID =
    Int

type alias Points =
    Int

demo : Model
demo =
    { units =
        [ (1, Battle.Unit.infantry (World.Position.init 4 2 0))
        , (2, Battle.Unit.infantry (World.Position.init 7 2 0))
        , (3, Battle.Unit.infantry (World.Position.init 6 1 0))
        , (4, Battle.Unit.infantry (World.Position.init 5 4 0))
        ]
    , nextUnitID = 5
    , selectedUnitID = 0
    }

getSelectedUnit : Model -> (ID, Battle.Unit.Model)
getSelectedUnit model =
    model.units
        |> List.filter (\(id, u) -> id == model.selectedUnitID)
        |> List.head

cycleUnits : Model -> (ID, Battle.Unit.Model)
cycleUnits model =
    model.units
        |> List.head

cycleUnitsWithPoints : Model -> (ID, Battle.Unit.Model)
cycleUnitsWithPoints model =
    let selectedUnitID =
            if model.selectedUnitID >= List.length model.units
                then 0
                else model.selectedUnitID
    in
        model.units
            |> List.filter (\(id, u) -> Battle.Unit.hasPoints u)
            |> List.filter (\(id, u) -> id > selectedUnitID)
            |> List.head

getNextUnitWithPoints : Model -> (ID, Battle.Unit.Model)
getNextUnitWithPoints model =
    List.head <| model.units


-- UPDATE

type Action
    = NoOp
    | SelectUnit ID
    | SetSelectedUnit ID
    | UnselectUnit ID
    | UnselectAll
    | SelectUnitAtPosition World.Position.Model
    | MoveUnit ID World.Position.Model
    | Offset (Float, Float)
    | ModifyUnit ID Battle.Unit.Action


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        SelectUnit id ->
            model |> update UnselectAll
                  |> update (ModifyUnit id Battle.Unit.Select)
                  |> update (SetSelectedUnit id)

        SetSelectedUnit id ->
            { model | selectedUnitID <- id }

        UnselectUnit id ->
            model |> update (ModifyUnit id Battle.Unit.Unselect)

        UnselectAll ->
            let unselectUnit (unitID, unitModel) =
                    (unitID, Battle.Unit.update Battle.Unit.Unselect unitModel)
            in
                { model | units <- model.units |> List.map unselectUnit }

        SelectUnitAtPosition pos ->
            let selectUnit (unitID, unitModel) =
                    if unitModel.pos == pos
                        then (unitID, Battle.Unit.update Battle.Unit.ToggleSelect unitModel)
                        else (unitID, unitModel)
            in
                { model | units <- model.units |> List.map selectUnit }

        MoveUnit id target ->
            let moveUnit (unitID, unitModel) =
                    if unitID == id
                        then (unitID, Battle.Unit.update (Battle.Unit.Place target) unitModel)
                        else (unitID, unitModel)
            in
                { model | units <- model.units |> List.map moveUnit }

        Offset offset ->
            model

        ModifyUnit id unitAction ->
            let updateUnit (unitID, unitModel) =
                    if unitID == id
                        then (unitID, Battle.Unit.update unitAction unitModel)
                        else (unitID, unitModel)
            in
                { model | units <- model.units |> List.map updateUnit }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let forms =
            model.units |> List.map (viewUnit context)
    in
        forms |> Graphics.Collage.group

viewUnit : Context -> (ID, Battle.Unit.Model) -> Graphics.Collage.Form
viewUnit context (id, unit) =
    let context' =
            Battle.Unit.Context
                (LocalChannel.localize (ModifyUnit id) context.actionChannel)
    in
        Battle.Unit.view context' unit