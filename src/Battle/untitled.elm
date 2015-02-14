type alias Node =
    { x : Int
    , y : Int
    , cost : Int
    }

getShortestPath : Node -> Node -> List Node -> List Node
getShortestPath start finish grid =
    let helper frontier visited =
            map (\f ->
                helper
                    (getNeighbours f grid)
                    (f :: visited)
            ) frontier
    in
        helper (getNeighbours start grid) [start]

getNeighbours : Node -> List Node -> List Node
getNeighbours current grid =
    grid |> map (isNeighbour current)