module Editor.Editor where

import Signal
import World.World
import Editor.Tool
import LocalChannel
import World.Position
import Editor.Interface
import Graphics.Collage


-- MODEL

type alias Model =
    { world : World.World.Model
    , interface : Editor.Interface.Model
    }

default : Model
default =
    { world = World.World.default
    , interface = Editor.Interface.default
    }


-- UPDATE

type Action
    = NoOp
    | ModifyWorld World.World.Action
    | ModifyInterface Editor.Interface.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ModifyWorld worldAction ->
            { model
                | world <- model.world |> World.World.update worldAction
            }

        ModifyInterface interfaceAction ->
            { model
                | interface <- model.interface |> Editor.Interface.update interfaceAction
            }

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
view context model =
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