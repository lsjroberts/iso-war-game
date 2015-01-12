module Editor.Assets where

import Editor.Model (..)

brushes =
    --{ elevation = "/assets/editor/brushes/elevation.png"
    --, river = "/assets/editor/brushes/river.png" }
    { elevation = "/assets/world/tiles/landscapeTiles_075.png"
    , river = "/assets/world/tiles/landscapeTiles_019.png" }

getBrushSrc : BrushType -> String
getBrushSrc brush =
    case brush of
        Elevation -> brushes.elevation
        River -> brushes.river