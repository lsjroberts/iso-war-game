module Helpers where

nearEdge : Float -> (Int,Int) -> (Int,Int) -> (Float,Float)
nearEdge spacing (w,h) (x,y) =
    ( nearEdgeSide spacing (toFloat w) (toFloat x)
    , nearEdgeSide spacing (toFloat h) (toFloat y) )

nearEdgeSide : Float -> Float -> Float -> Float
nearEdgeSide spacing dim pos =
    let diff = dim - pos
    in if | diff < spacing -> 1 - diff / spacing
          | pos < spacing  -> pos / spacing - 1
          | otherwise      -> 0