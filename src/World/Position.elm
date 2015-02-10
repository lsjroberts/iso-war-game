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
            [0..h-1] |> List.map (\y -> init x y 0)
                   |> List.reverse
    in [0..w-1] |> List.concatMap (\x -> column x)

circle : Int -> Model -> List Model
circle r centre =
    let column x =
            [(0-r)..r]
                |> List.map (\y -> init x (y + centre.y) 0)
                |> List.reverse
    in
        [(0-r)..r]
            |> List.concatMap (\x -> column (x + centre.x))

-- TODO: Refactor this to be a configurable value
zoom : Float
zoom = 0.5

tileWidth : Int
tileWidth = 131

tileHeight : Int
tileHeight = 64

tileVerticalSpacing : Float
tileVerticalSpacing = 16.0

translateToScreen : Model -> (Float, Float)
translateToScreen ({x, y, z} as model) =
    let halfWidth = (toFloat tileWidth) * zoom / 2
        halfHeight = (toFloat tileHeight) * zoom / 2
        x' = toFloat x
        y' = toFloat y
        z' = toFloat z
    in
        ( x'*halfWidth + y'*halfWidth
        , y'*halfHeight - x'*halfHeight + z'*tileVerticalSpacing
        )

translateScreenToPos : (Float, Float) -> Model
translateScreenToPos (x, y) =
    let halfWidth = (toFloat tileWidth) * zoom / 2
        halfHeight = (toFloat tileHeight) * zoom / 2
        offsetX = 698
        offsetY = 371
        x' = (x-offsetX)/halfWidth - (offsetY-y)/halfWidth |> floor
        y' = (offsetY-y)/halfHeight + (x-offsetX)/halfHeight |> floor
        --x' = (x-offsetX)/halfWidth |> floor
        --y' = (y-offsetY)/(0-halfHeight) |> floor
        --x' = (x-offsetX)/halfWidth + (y-offsetY)/halfWidth |> floor
        --y' = (y-offsetY)/(halfHeight) - (x-offsetX)/halfHeight + 0 |> floor
        z' = 0
    in
        init x' y' z'

move : (Int, Int, Int) -> Model -> Model
move (x, y, z) model =
    { model
        | x <- model.x + x
        , y <- model.y + y
        , z <- model.z + z
    }