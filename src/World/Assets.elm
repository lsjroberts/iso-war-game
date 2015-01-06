module World.Assets where

import World.Model (..)

tiles =
    { grass   = "/assets/world/tiles/landscapeTiles_067.png"
    , dirt    = "/assets/world/tiles/landscapeTiles_073.png"

    , hillTop = "/assets/world/tiles/landscapeTiles_075.png"
    , hillN   = "/assets/world/tiles/landscapeTiles_099.png"
    , hillNE  = "/assets/world/tiles/landscapeTiles_029.png"
    , hillE   = "/assets/world/tiles/landscapeTiles_106.png"
    , hillSE  = "/assets/world/tiles/landscapeTiles_036.png"
    , hillS   = "/assets/world/tiles/landscapeTiles_098.png"
    , hillSW  = "/assets/world/tiles/landscapeTiles_028.png"
    , hillW   = "/assets/world/tiles/landscapeTiles_091.png"
    , hillNW  = "/assets/world/tiles/landscapeTiles_021.png" }

getTileImageSrc : TileType -> String
getTileImageSrc tileType =
    --"/assets/test-tile.png"
    case tileType of
        GrassTile   -> tiles.grass
        DirtTile    -> tiles.dirt
        HillTopTile -> tiles.hillTop
        HillNTile   -> tiles.hillN
        HillNETile  -> tiles.hillNE
        HillETile   -> tiles.hillE
        HillSETile  -> tiles.hillSE
        HillSTile   -> tiles.hillS
        HillSWTile  -> tiles.hillSW
        HillWTile   -> tiles.hillW
        HillNWTile  -> tiles.hillNW