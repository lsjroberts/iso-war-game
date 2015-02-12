module Battle.Battle where

import Debug (log)

import List
import Signal
import LocalChannel
import Graphics.Collage

import Battle.Army
import Battle.Player

import World.World
import World.Position


-- MODEL

type alias Model =
    { world : World.World.Model
    , players : List (ID, Battle.Player.Model)
    , offset : (Float, Float)
    }

type alias ID =
    Int

default : Model
default =
    { world = World.World.demo
    , players = [(1, Battle.Player.demoHuman)]
    , offset = (0, 0)
    }


-- UPDATE

type Action
    = NoOp
    | Offset (Float, Float)
    | ModifyWorld World.World.Action
    --| ModifyInterface Editor.Interface.Action
    | ModifyPlayer ID Battle.Player.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Offset (x', y') ->
            let (x, y) = model.offset
                x'' = x - (x' * 20)
                y'' = y + (y' * 20)
            in
                { model
                    | offset <- (x'', y'')
                }
--                |> update (ModifyWorld (World.World.Offset offset))
--                |> update (ModifyPlayer 1 (Battle.Player.ModifyArmy (Battle.Army.Offset offset)))

        ModifyWorld worldAction ->
            { model
                | world <- model.world |> World.World.update worldAction
            }

        --ModifyInterface interfaceAction ->
        --    { model
        --        | interface <- model.interface |> Editor.Interface.update interfaceAction
        --    }

        ModifyPlayer id playerAction ->
            let updatePlayer (playerID, playerModel) =
                    if playerID == id
                        then (playerID, Battle.Player.update playerAction model.world playerModel)
                        else (playerID, playerModel)
            in
                { model |
                    players <- model.players |> List.map updatePlayer
                }

step : Model -> Model
step ({world} as model) =
    let world' =
            world -- |> Editor.Tool.paint interface.tool

        --interface' =
        --    interface
    in
        { model
            | world <- world'
            --, interface <- interface'
        }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let forms =
            [ viewWorld context model.world ]
            --, viewInterface context model.interface
            ++ (model.players |> List.map (viewPlayer context))
    in
        forms
            |> Graphics.Collage.group
            |> Graphics.Collage.move model.offset

viewWorld : Context -> World.World.Model -> Graphics.Collage.Form
viewWorld context world =
    let context' =
            World.World.Context
                (LocalChannel.localize (ModifyWorld) context.actionChannel)
    in
        World.World.view context' world

viewPlayer : Context -> (ID, Battle.Player.Model) -> Graphics.Collage.Form
viewPlayer context (id, player) =
    let context' =
            Battle.Player.Context
                (LocalChannel.localize (ModifyPlayer id) context.actionChannel)
    in
        Battle.Player.view context' player