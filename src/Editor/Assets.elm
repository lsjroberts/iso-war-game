module Editor.Assets where

import Editor.Brushes

brushes =
    --{ elevation = "/assets/editor/brushes/elevation.png"
    --, river = "/assets/editor/brushes/river.png" }
    { river = "/assets/world/tiles/landscapeTiles_019.png"
    , elevation = "/assets/world/tiles/landscapeTiles_075.png" }

getBrushSrc : Editor.Brushes.BrushType -> String
getBrushSrc brush =
    case brush of
        Editor.Brushes.River ->
            brushes.river
        Editor.Brushes.Elevation ->
            brushes.elevation