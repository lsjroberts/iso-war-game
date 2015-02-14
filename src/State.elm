module State where

import Debug (log,watch)
import Signal ((<~))

import Time
import Mouse
import Signal
import Window
import Keyboard
import LocalChannel
import Graphics.Collage
import Graphics.Element

import Helpers

import Battle.Battle
import Battle.Cursor
import Battle.Player

import Editor.Tool
import Editor.Editor
import Editor.Interface

import World.Position


-- MODEL

type State = Play | Pause | Editor

type alias Model =
    { state : State
    , battle : Maybe Battle.Battle.Model
    , editor : Maybe Editor.Editor.Model
    }

default : Model
default =
    { state = Play
    , battle = Nothing
    , editor = Nothing
    }


---- UPDATE

type Action
    = NoOp
    | TimeDelta Float
    | NearEdge (Float, Float)
    | MouseMove (Int, Int)
    | MouseDown Bool
    | KeysDown (List Int)
    | KeyPressed Int
    | ChangeState State
    | ModifyEditor Editor.Editor.Action
    | ModifyBattle Battle.Battle.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        TimeDelta delta ->
            let editor =
                    if model.state == Editor
                        then
                            case model.editor of
                                Nothing ->
                                    Just (Editor.Editor.default) -- TODO: This should return Nothing
                                Just editor ->
                                    Just (Editor.Editor.step editor)
                        else
                            Nothing

                battle =
                    if model.state == Play
                        then
                            case model.battle of
                                Nothing ->
                                    Just (Battle.Battle.default) -- TODO: This should return Nothing
                                Just battle ->
                                    Just (Battle.Battle.step battle)
                        else
                            Nothing
            in
                { model
                    | editor <- editor
                    , battle <- battle
                }

        NearEdge near ->
            nearEdge near model

        MouseMove move ->
            mouseMove move model

        MouseDown isDown ->
            mouseDown isDown model

        KeysDown keys ->
            keysDown keys model

        KeyPressed key ->
            keyPressed key model

        ChangeState state ->
            let editor =
                    if state == Editor
                        then Just (Editor.Editor.default)
                        else Nothing
                battle =
                    if state == Play
                        then Just (Battle.Battle.default)
                        else Nothing
            in
                { model
                    | state <- log "ChangeState" state
                    , editor <- editor
                    , battle <- battle
                }

        ModifyEditor editorAction ->
            let updateEditor editor =
                    case editor of
                        Nothing -> Nothing
                        Just editor ->
                            Just (Editor.Editor.update (log "editorAction" editorAction) editor)
            in
                { model | editor <- updateEditor model.editor }

        ModifyBattle battleAction ->
            let updateBattle battle =
                    case battle of
                        Nothing -> Nothing
                        Just battle ->
                            Just (Battle.Battle.update (log "battleAction" battleAction) battle)
            in
                { model | battle <- updateBattle model.battle }


---- VIEW

view : (Int, Int) -> Model -> Graphics.Element.Element
view (w, h) ({state, editor, battle} as gameState) =
    let forms =
            if | state == Editor -> viewEditor editor
               | state == Play   -> viewBattle battle
               | otherwise       -> [ ]
    in
        forms |> Graphics.Collage.collage w h
              |> Graphics.Element.container w h Graphics.Element.middle

viewEditor : Maybe Editor.Editor.Model -> List Graphics.Collage.Form
viewEditor editor =
    let context =
            Editor.Editor.Context
                (LocalChannel.create (ModifyEditor) actionChannel)
    in
        case editor of
            Nothing ->
                [ ]
            Just editor ->
                [ Editor.Editor.view context editor ]

viewBattle : Maybe Battle.Battle.Model -> List Graphics.Collage.Form
viewBattle battle =
    let context =
            Battle.Battle.Context
                (LocalChannel.create (ModifyBattle) actionChannel)
    in
        case battle of
            Nothing ->
                [ ]
            Just battle ->
                [ Battle.Battle.view context battle ]


-- SIGNALS

model : Signal.Signal Model
model =
    watch "State" <~
    Signal.foldp update default input

input : Signal.Signal Action
input =
    Signal.mergeMany
        [ Signal.subscribe actionChannel
        --, Signal.map NearEdge (Signal.sampleOn (Time.fps 30) (Mouse.position |> Signal.map2 (Helpers.nearEdge 40) Window.dimensions))
        , Signal.map MouseMove (Mouse.position)
        , Signal.map MouseDown (Mouse.isDown)
        , Signal.map KeyPressed (Keyboard.lastPressed)
        , Signal.map KeysDown (Keyboard.keysDown)
        , Signal.map TimeDelta (Time.fps 30)
        ]

actionChannel : Signal.Channel Action
actionChannel =
    Signal.channel NoOp


-- INPUT

nearEdge : (Float, Float) -> Model -> Model
nearEdge near model =
    model |> nearEdgeBattle near

mouseMove : (Int, Int) -> Model -> Model
mouseMove move model =
    model |> mouseMoveEditor move

mouseDown : Bool -> Model -> Model
mouseDown isDown model =
    model |> mouseDownEditor isDown

keysDown : List Int -> Model -> Model
keysDown keys model =
    model

keyPressed : Int -> Model -> Model
keyPressed key model =
    model |> keyPressedBattle key


nearEdgeBattle : (Float, Float) -> Model -> Model
nearEdgeBattle near model =
    model |> modifyBattle (Battle.Battle.Offset near)

mouseMoveEditor : (Int, Int) -> Model -> Model
mouseMoveEditor (x, y) model =
    let x' = toFloat x
        y' = toFloat y
        pos = World.Position.translateScreenToPos (x', y')
    in
        model |> modifyEditorTool (Editor.Tool.Place pos)

mouseDownEditor : Bool -> Model -> Model
mouseDownEditor isDown model =
    if isDown
        then model |> modifyEditorTool Editor.Tool.Paint
        else model

keyPressedBattle : Int -> Model -> Model
keyPressedBattle key model =
    if | key == 190 ->
            model |> modifyHuman Battle.Player.SelectNextUnit
       | key == 37 ->
            model |> modifyCursor (Battle.Cursor.Move (-1, 0, 0))
       | key == 38 ->
            model |> modifyCursor (Battle.Cursor.Move (0, 1, 0))
       | key == 39 ->
            model |> modifyCursor (Battle.Cursor.Move (1, 0, 0))
       | key == 40 ->
            model |> modifyCursor (Battle.Cursor.Move (0, -1, 0))
       | key == 81 ->
            model |> modifyHuman Battle.Player.ToggleSelectedUnitCursorMode
       | key == 13 ->
            model |> modifyHuman Battle.Player.EnactCursor
       | otherwise ->
            model


-- MODIFY

modifyEditor action =
    update (ModifyEditor action)

modifyEditorInterface action =
    modifyEditor (Editor.Editor.ModifyInterface action)

modifyEditorTool action =
    modifyEditorInterface (Editor.Interface.ModifyTool action)


modifyBattle action =
    update (ModifyBattle action)

modifyHuman action =
    modifyBattle (Battle.Battle.ModifyPlayer 1 action)

modifyCursor action =
    modifyHuman (Battle.Player.ModifyCursor action)