module Editor.Editor where

import Input
import World.World
import Graphics.Collage


-- MODEL

type Action =
    NoOp

type alias Model =
    { world : World.World.Model
    }

default =
    { world = World.World.default
    }


-- UPDATE

handleInput : Input.Model -> Model -> Model
handleInput input model =
    model |> update NoOp

update : Action -> Model -> Model
update action model =
    model


-- VIEW

view : Model -> Graphics.Collage.Form
view ({world} as model) =
    let world' = World.World.view world
    in
        world'