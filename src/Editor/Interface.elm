module Editor.Interface where

import Debug (log)

import Html (..)
import List
import Html.Events (..)
import Editor.Tool
import LocalChannel
import Editor.Assets
import Editor.Brushes
import Graphics.Input
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
        [ Editor.Brushes.Grass
        , Editor.Brushes.Dirt
        , Editor.Brushes.River
        , Editor.Brushes.Elevation ]
    , tool = Editor.Tool.default }


-- UPDATE

type Action
    = NoOp
    | ModifyTool Editor.Tool.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ModifyTool toolAction ->
            { model
                | tool <- model.tool |> Editor.Tool.update toolAction
            }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context model =
    Graphics.Collage.group
        [ viewPanels context model
        , viewTool context model.tool
        ]

viewPanels : Context -> Model -> Graphics.Collage.Form
viewPanels context model =
    let panels =
        brushesPanel context model
    in
        section [] panels
            |> toElement 200 600
            |> Graphics.Collage.toForm

viewTool : Context -> Editor.Tool.Model -> Graphics.Collage.Form
viewTool context tool =
    let context' =
            Editor.Tool.Context
                (LocalChannel.localize (ModifyTool) context.actionChannel)
    in
        Editor.Tool.view context' tool

panel : Context -> Html -> Html
panel context inner =
    div [ class "editor-panel" ]
        [ inner ]

brushesPanel : Context -> Model -> List Html
brushesPanel context model =
    List.map (brush context model.tool.brushType) model.brushes

brush : Context -> Editor.Brushes.BrushType -> Editor.Brushes.BrushType -> Html
brush context selectedBrushType panelBrushType =
    img [ src (Editor.Assets.getBrushSrc panelBrushType)
        , onClick (LocalChannel.send context.actionChannel (ModifyTool (Editor.Tool.SetBrushType panelBrushType)))
        , style [ ("opacity", (if panelBrushType == selectedBrushType then "1" else "0.5")) ]
        ]
        [ ]