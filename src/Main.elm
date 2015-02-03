module Main where

import State
import Signal
import Window
import Graphics.Element

import Text
import Signal ((<~))

main : Signal.Signal Graphics.Element.Element
main =
    Signal.map2 State.view Window.dimensions State.model
    --Text.asText <~ State.model
