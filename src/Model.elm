module Model where

import Game.Model (Game)
import Editor.Model (Editor)

type State =
    Play | Pause | Editor

type alias Offset =
    (Float, Float)

type alias GameState =
    { state:State
    , offset:Offset
    , game:Maybe Game
    , editor:Maybe Editor }

defaultGame : GameState
defaultGame =
    { state = Editor
     , offset = (0,0)
     , game = Nothing
     , editor = Nothing }