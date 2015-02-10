module Input where

import Time
import Debug
import Mouse
import Signal
import Signal ((<~))
import Window


-- INPUT

type alias Model =
    { delta:Float
    , userInput:UserInput }

input : Signal.Signal Model
input =
    Debug.watch "input" <~
    Signal.sampleOn delta (Signal.map2 Model delta userInput)


-- TIME INPUT

delta : Signal.Signal Float
delta = Time.fps 30


-- USER INPUT

type alias UserInput =
    { scroll:(Float,Float)
    , mousePos:(Int,Int)
    , isMouseDown:Bool }

scroll : Signal.Signal (Float,Float)
scroll = Mouse.position |> Signal.map2 nearWindowEdge Window.dimensions -- refactor to generic `nearEdge dimensions spacing`

mousePos : Signal.Signal (Int,Int)
mousePos = Mouse.position

isMouseDown : Signal.Signal Bool
isMouseDown = Mouse.isDown

nearWindowEdge : (Int,Int) -> (Int,Int) -> (Float,Float)
nearWindowEdge (w,h) (x,y) =
    ( nearWindowEdgeSide (toFloat w) (toFloat x)
    , nearWindowEdgeSide (toFloat h) (toFloat y) )

nearWindowEdgeSide : Float -> Float -> Float
nearWindowEdgeSide dim pos =
    let spacing = 20
        diff = dim - pos
    in if | diff < spacing -> 1 - diff / spacing
          | pos < spacing  -> pos / spacing - 1
          | otherwise      -> 0

userInput : Signal.Signal UserInput
userInput =
    Signal.map3 UserInput scroll mousePos isMouseDown