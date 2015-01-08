module Game.Display (displayPlay,displayPause) where

import Graphics.Collage (Form,group,move)

import Model (GameState)
import Game.Model (Game)
import Game.World.Display (displayWorld)

displayPlay : (Int,Int) -> GameState -> List Form
displayPlay dimensions ({game} as gameState) =
    case game of
        Nothing -> []
        Just game -> game |> displayGame dimensions

displayGame : (Int,Int) -> Game -> List Form
displayGame dimensions ({world,offset} as game) =
    [ displayWorld dimensions world |> group
                                    |> move offset ]

displayPause : (Int,Int) -> GameState -> List Form
displayPause dimensions gameState =
    []