app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D
import array2d.Index2D

example =
    """
    ###############
    #.......#....E#
    #.#.###.#.###.#
    #.....#.#...#.#
    #.###.#####.#.#
    #.#.#.......#.#
    #.#.#####.###.#
    #...........#.#
    ###.#.#####.#.#
    #...#.....#.#.#
    #.#.#.###.#.#.#
    #.....#...#.#.#
    #.###.#.#.#.#.#
    #S..#.....#...#
    ###############
    """

expect
    got = part1 example
    expected = Ok "7036"
    got == expected

part1 = \input ->
    { array, start, end } = input |> parse?

    startNode = { index: start, direction: East }
    isGoal = \{ index } -> index == end

    getNeighbors = \{ index, direction }, seen ->
        (left, right) = turn index direction
        all =
            when mayGo array index direction is
                Ok path ->
                    [path, left, right]

                Err _ ->
                    [left, right]
        all
        |> List.dropIf \f -> List.contains seen f.node

    shortestPath? startNode isGoal getNeighbors
    |> Num.toStr
    |> Ok

parse = \input ->
    array =
        input
        |> Str.splitOn "\n"
        |> List.map \line ->
            line
            |> Str.toUtf8
        |> Array2D.fromLists FitShortest

    start = array |> Array2D.findFirstIndex? \e -> e == 'S'
    end = array |> Array2D.findFirstIndex? \e -> e == 'E'
    Ok { array, start, end }

mayGo = \array, index, direction ->
    nIndex = nextIndex? array index direction
    when Array2D.get array nIndex is
        Ok '#' -> Err Wall
        Err _ -> Err Invalid
        Ok _ ->
            Ok { node: { index: nIndex, direction }, distance: 1 }

nextIndex = \array, index, direction ->
    shape = array |> Array2D.shape
    when direction is
        East -> Index2D.incCol index shape
        South -> Index2D.incRow index shape
        West -> Index2D.decCol index shape
        North -> Index2D.decRow index shape

Direction : [East, South, West, North]

turn = \index, direction ->
    (
        { node: { index, direction: (direction |> turnRight) }, distance: 1000 },
        { node: { index, direction: (direction |> turnLeft) }, distance: 1000 },
    )

turnRight : Direction -> Direction
turnRight = \direction ->
    when direction is
        East -> South
        South -> West
        West -> North
        North -> East

turnLeft = \direction ->
    when direction is
        East -> North
        South -> East
        West -> South
        North -> West

Node a : a where a implements Eq
NodeDistance a : { distance : U64, node : Node a }
# TODO: Why does GetNeighbors need the `seen` argument? Why is it not possible to filter seen elements in shortestPathHelper?
GetNeighbors a : Node a, List (Node a) -> List (NodeDistance a)
IsGoal a : Node a -> Bool

shortestPath : Node a, IsGoal a, GetNeighbors a -> Result U64 [NotFound]
shortestPath = \start, goal, getNeighbors ->
    shortestPathHelper [{ distance: 0, node: start }] [] goal getNeighbors

shortestPathHelper : List (NodeDistance a), List (Node a), IsGoal a, GetNeighbors a -> Result U64 [NotFound]
shortestPathHelper = \list, seen, isGoal, getNeighbors ->
    when list is
        [] -> Err NotFound
        [{ distance, node }, .. as rest] ->
            if isGoal node then
                Ok distance
                else

            newSeen : List (Node a)
            newSeen = seen |> List.append node

            node
            |> getNeighbors newSeen
            # |> List.dropIf \neighbor -> List.contains newSeen neighbor.node
            |> List.map \nodeDistance -> { nodeDistance & distance: nodeDistance.distance + distance }
            |> List.walk rest \acc, neighbor -> updateNodeDistance acc neighbor 0
            |> List.sortWith \{ distance: a }, { distance: b } -> Num.compare a b
            |> shortestPathHelper newSeen isGoal getNeighbors

updateNodeDistance = \list, newNodeDistance, index ->
    when List.get list index is
        Err OutOfBounds ->
            List.append list newNodeDistance

        Ok existingNodeDistance if existingNodeDistance.node == newNodeDistance.node ->
            if newNodeDistance.distance < existingNodeDistance.distance then
                List.set list index newNodeDistance
            else
                list

        _ ->
            updateNodeDistance list newNodeDistance (index + 1)

# expect
#    got = part2 example
#    expected = Ok "TODO"
#    got == expected

part2 = \_input ->
    Err TODO
