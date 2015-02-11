import List (..)

type alias Node =
    { x : Int
    , y : Int
    }

type alias Cost =
    Int

type alias Costed =
    ( Node
    , Cost
    )

type alias Graph =
    List Node

type alias Visited =
    List Costed

type alias Unvisited =
    List Costed

type alias Sets =
    ( Unvisited
    , Visited
    )

dijkstra : Node -> Graph -> Graph
dijkstra current graph =
    graph
        |> init current
        |> calculateNeighbourCosts current

init : Node -> Graph -> Sets
init start graph =
    graph
        |> map (\node ->
            if node == start
                then (0, node)
                else (999, node)
        )
        |> partition (\(cost, node) ->
            if node == start
                then False
                else True
        )

calculateNeighbourCosts : Node -> Sets -> Sets
calculateNeighbourCosts node sets =



----

type alias Cost = Int
type alias Current = (Cost, Model)
type alias Visited = (Cost, Model)
type alias Unvisited = (Cost, Model)
type alias DijkstraPartition = (List Visited, List Unvisited)

areaWithCost : Model -> Int -> World.Position.Model -> List Visited
areaWithCost ({tiles} as model) points fromPos =
    let fromTile = getTileAtPos fromPos
    in
        tiles
            |> dijkstraInitialise fromTile
            |> dijkstraVisitAll


dijkstraInitialise : World.Tile.Model -> List (ID, World.Tile.Model) -> DijkstraPartition
dijkstraInitialise fromTile tiles =
    tiles
        |> List.map (\tile -> if tile == fromTile then (0, tile) else (999, tile))
        |> List.partition (\(cost, tile) -> if tile == fromTile then True else False)

dijkstraVisitAll : List Unvisited -> List Visited
dijkstraVisitAll unvisited =
    unvisited
        |> List.map dijkstraVisit (unvisited, [])

dijkstraVisit : Current -> DijkstraPartition -> DijkstraPartition
dijkstraVisit current (unvisited, visited) =
    let check (unvisited, visited) =
            if List.length unvisited > 0
                then dijkstraVisit (List.head unvisited) (unvisited, visited)
                else (unvisited, visited)
    in
        unvisited
            |> List.filter (dijkstraIsNeighbour current)
            |> List.map (dijkstraCalculateCost current)
            |> dijkstraPartitionTransfer (unvisited, visited)
            |> check

dijkstraIsNeighbour : Current -> Unvisited -> Bool
dijkstraIsNeighbour (tileCost, tile) (checkCost, check) =
    let tilePos = tile.pos
        checkPos = check.pos
        diffX = tilePos.x - checkPos.x
        diffY = tilePos.y - checkPos.y
    in
        if diffX >= -1 && diffX <= 1 && diffY >= -1 && diffY <= 1
            then True
            else False

dijkstraCalculateCost : Current -> Unvisited -> Unvisited
dijkstraCalculateCost (tileCost, tile) (neighbourCost, neighbour) =
    let newCost = tileCost + 1 --neighbour.
    in
        if newCost < neighbourCost
            then (newCost, neighbour)
            else (neighbourCost, neighbour)

dijkstraPartitionTransfer : DijkstraPartition -> Unvisited -> DijkstraPartition
dijkstraPartitionTransfer (unvisited, visited) newVisited =
    ( unvisited |> List.filter (\u -> not (List.member newVisited u))
    , visited ++ newVisited
    )

tileCostFrom : Model -> World.Position.Model -> (Cost, World.Tile.Model) -> (Cost, World.Tile.Model)
tileCostFrom model fromPos (toID, toTile) =
    (1, toTile)