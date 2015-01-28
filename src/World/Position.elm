module World.Position where

import List


-- MODEL

type alias Model =
    { x : Int
    , y : Int
    , z : Int }

default : Model
default =
    { x = 0
    , y = 0
    , z = 0 }

init : Int -> Int -> Int -> Model
init x y z =
    { x = x
    , y = y
    , z = z }

area : (Int, Int) -> List Model
area (w, h) =
    let column x =
            [h-1..0] |> List.map (\y -> init x y 0)
    in [0..w-1] |> List.concatMap (\x -> column x)

-- TODO: Refactor this to be a configurable value
zoom : Float
zoom = 0.5

tileWidth : Int
tileWidth = 131

tileHeight : Int
tileHeight = 131

tileVerticalSpacing : Float
tileVerticalSpacing = 16.0

translateToScreenCoords : Model -> (Float, Float)
translateToScreenCoords ({x, y, z} as model) =
    let w = (toFloat tileWidth) * zoom / 2
        h = (toFloat tileHeight) * zoom / 2
        x' = toFloat x
        y' = toFloat y
        z' = toFloat z
    in
        ( x'*w + y'*w
        , y'*h - x'*h + z'*tileVerticalSpacing
        )