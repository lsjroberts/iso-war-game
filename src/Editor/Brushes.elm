module Editor.Brushes where

import World.Tile

-- MODEL

type BrushType
    = Grass
    | Dirt
    | River
    | Elevation


getTileTypeFromBrushType : BrushType -> World.Tile.TileType
getTileTypeFromBrushType brushType =
    case brushType of
        Grass -> World.Tile.GrassTile
        Dirt -> World.Tile.DirtTile
        River -> World.Tile.DirtTile
        Elevation -> World.Tile.HillSTile