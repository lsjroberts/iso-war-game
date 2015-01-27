module State where

import Input
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

update : Input.Model -> Model -> Model
update input ({state, editor} as gameState) =
    --if | state == Play   -> gameState |> Game.Updates.play input
    --   | state == Pause  -> gameState |> Game.Updates.pause input
    if | state == Editor -> gameState |> updateEditor input
       | otherwise -> gameState

updateEditor : Input.Model -> Model -> Model
updateEditor input ({editor} as gameState) =
    case editor of
        Nothing ->
            { gameState | editor <- Just Editor.Editor.default }
        Just editor ->
            { gameState | editor <- Just (editor |> Editor.Editor.handleInput input) }


---- VIEW

view : (Int,Int) -> Model -> Graphics.Element.Element
view (w,h) ({state} as gameState) =
    let forms =
        --if | state == Play   -> Game.View.play     (w,h) gameState
    --               | state == Pause  -> Game.View.pause    (w,h) gameState
        if | state == Editor -> viewEditor gameState
           | otherwise       -> []
    in Graphics.Element.container w h Graphics.Element.middle <| Graphics.Collage.collage w h
        forms

viewEditor : Model -> List Graphics.Collage.Form
viewEditor ({editor} as model) =
    case editor of
        Nothing -> []
        Just editor ->
            [ Editor.Editor.view editor ]