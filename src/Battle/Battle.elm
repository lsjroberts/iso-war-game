module Battle.Battle where

import Debug (log)

import List
import Signal
import World.World
import LocalChannel
import Battle.Player
import World.Position
import Graphics.Collage


-- MODEL

type alias Model =
    { world : World.World.Model
    , players : List (ID, Battle.Player.Model) }

type alias ID =
    Int

default : Model
default =
    { world = World.World.demo
    , players = [(1, Battle.Player.demoHuman)]
    }


-- UPDATE

type Action
    = NoOp
    | ModifyWorld World.World.Action
    --| ModifyInterface Editor.Interface.Action
    | ModifyPlayer ID Battle.Player.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

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
                        then (playerID, Battle.Player.update playerAction playerModel)
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

--handleKeysDown : List Int -> Model -> Model
--handleKeysDown keys model =
--    model --List.map (\key -> handleKeyDown key model) keys

--handleKeyDown : Int -> Model -> Model
--handleKeyDown key model =
--    model

--handleKeyPressed : Int -> Model -> Model
--handleKeyPressed key model =
--    let handleKeyPressedPlayer (playerID, playerModel) =
--            (playerID, playerModel |> Battle.Player.handleKeyPressed key)
--    in
--        { model
--            | players <- model.players |> List.map handleKeyPressedPlayer
--        }


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
        forms |> Graphics.Collage.group

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