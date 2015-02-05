module Editor.Interface where

import Debug (log)

import Html (..)
import List
import Editor.Tool
import LocalChannel
import Editor.Assets
import Editor.Brushes
import Html.Attributes (..)
import Graphics.Collage
import Graphics.Element


-- MODEL

type alias Model =
    { brushes : List Editor.Brushes.BrushType
    , tool : Editor.Tool.Model
    }

default : Model
default =
    { brushes =
        [ Editor.Brushes.River
        , Editor.Brushes.Elevation ]
    , tool = Editor.Tool.default }


-- UPDATE

type Action
    = NoOp


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    let panels =
        brushesPanel model
    in
        section [] (log "panels" panels)
            |> toElement 200 600
            |> Graphics.Collage.toForm

panel : Html -> Html
panel inner =
    div [ class "editor-panel" ]
        [ inner ]

brushesPanel : Model -> List Html
brushesPanel model =
    List.map brush model.brushes

brush : Editor.Brushes.BrushType -> Html
brush brushType =
    img [ src (Editor.Assets.getBrushSrc brushType) ] []