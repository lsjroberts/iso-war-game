module Editor.Editor where

import Input
import Signal
import World.World
import LocalChannel
import Graphics.Collage


-- MODEL

type alias Model =
    { world : World.World.Model
    }

default =
    { world = World.World.default
    }


-- UPDATE

type Action
    = NoOp
    | ModifyWorld World.World.Action

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

step : Model -> Model
step model =
    model


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({world} as model) =
    model.world |> viewWorld context

viewWorld : Context -> World.World.Model -> Graphics.Collage.Form
viewWorld context world =
    let context' =
            World.World.Context
                (LocalChannel.localize (ModifyWorld) context.actionChannel)
    in
        World.World.view context' world