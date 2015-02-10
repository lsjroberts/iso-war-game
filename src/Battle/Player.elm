module Battle.Player where

import Debug (log)

import Battle.Army
import LocalChannel
import Battle.Assets
import Battle.Pointer
import World.Position
import Graphics.Collage
import Graphics.Element


-- MODEL

type alias Model =
    { name : String
    , army : Battle.Army.Model
    , pointer : Battle.Pointer.Model
    }

demoHuman : Model
demoHuman =
    { name = "Human"
    , army = Battle.Army.demo
    , pointer = Battle.Pointer.standard
    }


-- UPDATE

type Action
    = NoOp
    | SelectNextUnit
    | ModifyArmy Battle.Army.Action
    | ModifyPointer Battle.Pointer.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        SelectNextUnit ->
            --model
            let (unitID, unitModel) =
                    Battle.Army.cycleUnitsWithPoints model.army
                pointer =
                    Battle.Pointer.update (Battle.Pointer.Place unitModel.pos) model.pointer
                army =
                    Battle.Army.update (Battle.Army.SelectUnit unitID) model.army
            in
                { model
                    | army <- army
                    , pointer <- pointer
                }

        ModifyArmy armyAction ->
            { model | army <- model.army |> Battle.Army.update armyAction }

        ModifyPointer pointerAction ->
            model
            --{ model | pointer <- model.pointer |> Battle.Pointer.update pointerAction }
            --let pointer = model.pointer |> Battle.Pointer.update (log "pointerAction" pointerAction)
            --    army =
            --        if pointerAction == Battle.Pointer.SelectUnit
            --            then Battle.Army.update (Battle.Army.SelectUnitAtPosition pointer.pos) model.army
            --            else model.army
            --in
            --    { model
            --        | pointer <- pointer
            --        , army <- army
            --    }

handleKeyPressed : Int -> Model -> Model
handleKeyPressed key model =
    let action =
            if | key == 190 -> SelectNextUnit
               | otherwise -> NoOp
        --model =
        --    model |> update action
    in
        model |> update action
        --{ model | pointer <- model.pointer |> Battle.Pointer.handleKeyPressed key }
    --{ model
    --    | pointer <- model.pointer |> Battle.Pointer.handleKeyPressed key
    --    , army <- model.army |> Battle.Army.handleKeyPressed key
    --}


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let forms =
            [ viewPointer context model.pointer
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

viewPointer : Context -> Battle.Pointer.Model -> Graphics.Collage.Form
viewPointer context pointer =
    let context' =
            Battle.Pointer.Context
                (LocalChannel.localize (ModifyPointer) context.actionChannel)
    in
        Battle.Pointer.view context' pointer