module Editor.Tool where

type Action
    = Paint World.Position.Model
    | PaintArea (World.Position.Model, World.Position.Model)
    | Clear

type alias Tool =
    {  }

update : Action -> Model -> Model
update action model =
    case action of
        Paint point -> -- ???

view : Model -> Form
view model =
    foo