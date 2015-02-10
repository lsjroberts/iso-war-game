module State where

import Debug (log,watch)
import Signal ((<~))

import Time
import Mouse
import Signal
import Keyboard
import LocalChannel
import Battle.Battle
import Editor.Editor
import Graphics.Collage
import Graphics.Element


-- MODEL

type State = Play | Pause | Editor

type alias Model =
    { state:State
    , battle:Maybe Battle.Battle.Model
    , editor:Maybe Editor.Editor.Model }

default : Model
default =
    { state = Play
    , battle = Nothing
    , editor = Nothing }


---- UPDATE

type Action
    = NoOp
    | TimeDelta Float
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

        MouseMove move ->
            let editor =
                    case model.editor of
                        Nothing -> Nothing
                        Just editor ->
                            Just (Editor.Editor.handleMouseMove move editor)
            in
                { model
                    | editor <- editor
                }

        MouseDown isDown ->
            let editor =
                    case model.editor of
                        Nothing -> Nothing
                        Just editor ->
                            Just (Editor.Editor.handleMouseDown isDown editor)
            in
                { model
                    | editor <- editor
                }

        KeysDown keys ->
            let battle =
                    case model.battle of
                        Nothing -> Nothing
                        Just battle ->
                            Just (Battle.Battle.handleKeysDown keys battle)
            in
                { model
                    | battle <- battle
                }

        KeyPressed key ->
            let battle =
                    case model.battle of
                        Nothing -> Nothing
                        Just battle ->
                            Just (Battle.Battle.handleKeyPressed key battle)
            in
                { model
                    | battle <- battle
                }

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
        , Signal.map MouseMove (Mouse.position)
        , Signal.map MouseDown (Mouse.isDown)
        , Signal.map KeyPressed (Keyboard.lastPressed)
        , Signal.map KeysDown (Keyboard.keysDown)
        , Signal.map TimeDelta (Time.fps 30)
        ]

actionChannel : Signal.Channel Action
actionChannel =
    Signal.channel NoOp