module Editor.Editor where

import Input
import Signal
import World.World
import Editor.Tool
import LocalChannel
import Editor.Interface
import Graphics.Collage


-- MODEL

type alias Model =
    { world : World.World.Model
    , interface : Editor.Interface.Model
    }

default =
    { world = World.World.default
    , interface = Editor.Interface.default
    }


-- UPDATE

type Action
    = NoOp
    | ModifyWorld World.World.Action
    | ModifyInterface Editor.Interface.Action

--handleInput : Input.Model -> Model -> Model
--handleInput input model =
--    model |> update NoOp

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ModifyWorld worldAction ->
            { model
                | world <- World.World.update worldAction model.world
            }

        ModifyInterface interfaceAction ->
            model

step : Model -> Model
step ({world, interface} as model) =
    let world' =
            world |> Editor.Tool.paint interface.tool

        interface' =
            interface
    in
        { model
            | world <- world'
            , interface <- interface'
        }



-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({world} as model) =
    let forms =
            [ viewWorld context model.world
            , viewInterface context model.interface ]
    in
        forms |> Graphics.Collage.group

viewWorld : Context -> World.World.Model -> Graphics.Collage.Form
viewWorld context world =
    let context' =
            World.World.Context
                (LocalChannel.localize (ModifyWorld) context.actionChannel)
    in
        World.World.view context' world

viewInterface : Context -> Editor.Interface.Model -> Graphics.Collage.Form
viewInterface context interface =
    let context' =
            Editor.Interface.Context
                (LocalChannel.localize (ModifyInterface) context.actionChannel)
    in
        Editor.Interface.view context' interface