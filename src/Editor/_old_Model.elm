module Editor.Model where

import Game.World.Model (..)

type BrushType =
    Elevation | River | DenseUrban | SparseUrban | Road

type BrushProbability =
    Fixed | Possible | Never

type alias Brush =
    { brushType:BrushType
    , probability:BrushProbability
    , isPainting:Bool
    , pos:Position }

type alias Editor =
    { brush:Maybe Brush
    , world:Maybe World
    , saved:Maybe Bool }

defaultEditor : Editor
defaultEditor =
    { brush = Just { brushType = River, probability = Fixed, isPainting = False, pos = {x=0,y=0,z=0} } --brush = Nothing
    , world = Just (filled GrassTile 20 20 1)
    , saved = Nothing }