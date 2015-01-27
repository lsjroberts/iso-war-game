module World.Position where

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

translateToScreenCoords : Model -> (Int, Int)
translateToScreenCoords model =
    (0, 0)