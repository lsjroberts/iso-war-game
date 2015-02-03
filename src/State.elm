module State where

import Debug (log,watch)
import Signal ((<~))

import Time
import Input
import Signal
import LocalChannel
import Editor.Editor
import Graphics.Collage
import Graphics.Element
--import Game.Game


-- MODEL

type State = Play | Pause | Editor

type alias Model =
    { state:State
    --, game:Maybe Game.Game.Model
    , editor:Maybe Editor.Editor.Model }

default : Model
default =
    { state = Editor
    --, game = Nothing
    , editor = Nothing }


---- UPDATE

type Action
    = NoOp
    | TimeDelta Float
    | ChangeState State
    | ModifyEditor Editor.Editor.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        TimeDelta delta ->
            let editor =
                    case model.editor of
                        Nothing ->
                            Just (Editor.Editor.default) -- TODO: This should return Nothing
                        Just editor ->
                            Just (Editor.Editor.step editor)
            in
                { model
                    | editor <- editor
                }

        ChangeState state ->
            let editor = if state == Editor then Just (Editor.Editor.default) else Nothing
            in
                { model
                    | state <- log "ChangeState" state
                    , editor <- editor
                }

        ModifyEditor editorAction ->
            let updateEditor editor =
                    case editor of
                        Nothing -> Nothing
                        Just editor ->
                            Just (Editor.Editor.update (log "editorAction" editorAction) editor)
            in
                { model
                    | editor <- updateEditor model.editor
                }


---- VIEW

view : (Int, Int) -> Model -> Graphics.Element.Element
view (w, h) ({state, editor} as gameState) =
    let forms =
            if | state == Editor -> viewEditor editor
               | otherwise       -> []
    in
        Graphics.Element.container w h Graphics.Element.middle <| Graphics.Collage.collage w h
            forms

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


-- SIGNALS

model : Signal.Signal Model
model =
    watch "State" <~
    Signal.foldp update default
        (Signal.merge
            (Signal.subscribe actionChannel)
            (Signal.map TimeDelta (Time.fps 30))
        )

actionChannel : Signal.Channel Action
actionChannel =
    Signal.channel NoOp