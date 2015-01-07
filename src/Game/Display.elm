module Game.Display (displayPlay,displayPause) where

import Graphics.Collage (Form,group,move)

import Model (GameState)
import Game.Model (Game)
import Game.World.Display (displayWorld)

displayPlay : (Int,Int) -> GameState -> List Form
displayPlay dimensions ({game,offset} as gameState) =
    case game of
        Nothing -> []
        Just game -> game |> displayGame dimensions offset

displayGame : (Int,Int) -> (Float,Float) -> Game -> List Form
displayGame dimensions offset game =
    [ displayWorld dimensions game |> group
                                   |> move offset ]

displayPause : (Int,Int) -> GameState -> List Form
displayPause dimensions gameState =
    []