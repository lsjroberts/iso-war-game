module Input where

import Time
import Debug
import Mouse
--import Signal (Signal,map,map2,sampleOn)
import Signal (..)
import Window

type alias UserInput =
    { scroll:(Float,Float)
    , isMouseDown:Bool }

type alias Input =
    { delta:Float
    , userInput:UserInput }

delta : Signal Float
delta =
    Time.fps 30

scroll : Signal (Float,Float)
scroll =
    Mouse.position |> map2 nearWindowEdge Window.dimensions -- refactor to generic `nearEdge dimensions spacing`

isMouseDown : Signal Bool
isMouseDown =
    Mouse.isDown

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

userInput : Signal UserInput
userInput =
    map2 UserInput scroll isMouseDown

input : Signal Input
input =
    Debug.watch "input" <~
    sampleOn delta (map2 Input delta userInput)