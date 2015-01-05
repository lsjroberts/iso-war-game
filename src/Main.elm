module Main where

import Signal
import Window
import Graphics.Element (Element)

import Input
import Model
import Display
import Updates

gameState : Signal Model.GameState
gameState =
    Signal.foldp Updates.step Model.defaultGame Input.input

main : Signal Element
main =
    Signal.map2 Display.display Window.dimensions gameState