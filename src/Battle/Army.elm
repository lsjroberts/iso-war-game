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
        [ (1, Battle.Unit.infantry (World.Position.init 1 1 0))
        , (2, Battle.Unit.infantry (World.Position.init 2 1 0))
        , (3, Battle.Unit.infantry (World.Position.init 2 2 0))
        , (4, Battle.Unit.infantry (World.Position.init 3 2 0))
        ]
    , nextUnitID = 5
    , selectedUnitID = 0
    }

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
    | ModifyUnit ID Battle.Unit.Action
    | SelectUnit ID
    | SetSelectedUnit ID
    | UnselectUnit ID
    | UnselectAll
    | SelectUnitAtPosition World.Position.Model


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ModifyUnit id unitAction ->
            let updateUnit (unitID, unitModel) =
                    if unitID == id
                        then (unitID, Battle.Unit.update unitAction unitModel)
                        else (unitID, unitModel)
            in
                { model | units <- model.units |> List.map updateUnit }

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
                { model
                    | units <- model.units |> List.map selectUnit
                }

--handleKeyPressed : Int -> Model -> Model
--handleKeyPressed key model =
--    let handleKeyPressedUnit (unitID, unitModel) =
--            (unitID, unitModel |> Battle.Unit.handleKeyPressed key)
--    in
--        { model
--            | units <- model.units |> List.map handleKeyPressedUnit
--        }


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