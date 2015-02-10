module Battle.Pointer where

import LocalChannel
import Battle.Assets
import World.Position
import Graphics.Collage
import Graphics.Element


-- MODEL

type PointerType
    = Standard

type alias Model =
    { pointerType : PointerType
    , pos : World.Position.Model
    }

standard : Model
standard =
    { pointerType = Standard
    , pos = World.Position.init 0 0 0
    }


-- UPDATE

type Action
    = NoOp
    | Place World.Position.Model
    | Move (Int, Int, Int)
    | SelectUnit

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Place pos ->
            { model | pos <- pos }

        Move directions ->
            let pos' = model.pos |> World.Position.move directions
            in
                { model | pos <- pos' }

        SelectUnit ->
            model

handleKeyPressed : Int -> Model -> Model
handleKeyPressed key model =
    let action =
            if | key == 37 -> Move (-1, 0, 0)
               | key == 38 -> Move (0, 1, 0)
               | key == 39 -> Move (1, 0, 0)
               | key == 40 -> Move (0, -1, 0)
               | key == 13 -> SelectUnit
               | otherwise -> NoOp
    in
        model-- |> update action


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let coords = World.Position.translateToScreen model.pos
    in
        model
            |> image
            |> Graphics.Collage.toForm
            |> Graphics.Collage.move coords

image : Model -> Graphics.Element.Element
image model =
    let w = floor <| (toFloat 131) * 0.5
        h = floor <| (toFloat 131) * 0.5
        path = pointerPath model.pointerType
    in
        path |> Graphics.Element.image w h

pointerPath : PointerType -> String
pointerPath pointerType =
    let pointers = Battle.Assets.pointers
    in
        case pointerType of
            Standard -> pointers.standard