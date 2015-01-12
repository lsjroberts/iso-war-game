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
    , pos:Maybe Position }

type alias Editor =
    { brush:Maybe Brush
    , world:Maybe World
    , saved:Maybe Bool }

defaultEditor : Editor
defaultEditor =
    { brush = Just { brushType = River, probability = Fixed, isPainting = False, pos = Nothing } --brush = Nothing
    , world = Just (filled GrassTile 20 20 1)
    , saved = Nothing }