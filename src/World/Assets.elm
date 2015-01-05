module World.Assets where

import World.Model (..)

tiles =
    { grass = "/assets/world/tiles/landscapeTiles_067.png"
    , dirt  = "/assets/world/tiles/landscapeTiles_073.png" }

getTileImageSrc : TileType -> String
getTileImageSrc tileType =
    --"/assets/test-tile.png"
    case tileType of
        GrassTile -> tiles.grass
        DirtTile  -> tiles.dirt