module Editor.Assets where

import Editor.Brushes

brushes =
    --{ elevation = "/assets/editor/brushes/elevation.png"
    --, river = "/assets/editor/brushes/river.png" }
    { grass     = "/assets/world/tiles/landscapeTiles_067.png"
    , dirt      = "/assets/world/tiles/landscapeTiles_083.png"
    , river     = "/assets/world/tiles/landscapeTiles_019.png"
    , elevation = "/assets/world/tiles/landscapeTiles_098.png" }

getBrushSrc : Editor.Brushes.BrushType -> String
getBrushSrc brush =
    case brush of
        Editor.Brushes.Grass -> brushes.grass
        Editor.Brushes.Dirt -> brushes.dirt
        Editor.Brushes.River -> brushes.river
        Editor.Brushes.Elevation -> brushes.elevation