module Model where

import Game.Model (Game)
import Editor.Model (Editor)

type State =
    Play | Pause | Editor

type alias GameState =
    { state:State
    , game:Maybe Game
    , editor:Maybe Editor }

defaultGameState : GameState
defaultGameState =
    { state = Play
    , game = Nothing
    , editor = Nothing }