module Main where

import Signal
import Window
import Graphics.Element
--import View
import Input
import State
--import Update

state : Signal.Signal State.Model
state = Signal.foldp State.update State.default Input.input

main : Signal.Signal Graphics.Element.Element
main = Signal.map2 State.view Window.dimensions state
