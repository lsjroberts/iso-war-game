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
        Paint point ->
            let paint (world) =
                { world | tileList <- World.TileList.update Insert GrassTile {x=0,y=1,z=0} }
            in
                { model | world <- paint model.world }

        PaintArea (topLeft, bottomRight) ->
            model

        Clear ->
            { model | world <- World.World.update Clear model.world }

view : Model -> Form
view model =
    foo