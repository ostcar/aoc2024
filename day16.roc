app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
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
    expected = Ok "TODO"
    got == expected

part1 = \input ->
    input
    |> parse?
    |> Inspect.toStr
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

walkMaze = \{ array, start, end } ->
    walkMazeHelper array end [(start, East)] []

walkMazeHelper : Array2D.Array2D U8, Index2D.Index2D, Queue, List IndexDirection -> Result U64 _
walkMazeHelper = \array, end, queue, seen ->
    ((pos, points), restQueue) = getShorts? queue

    if pos == end then
        Ok points
        else

    unseenNeighbors =
        getNeighbors array pos
        |> List.dropIf \(index, direction, _) -> List.contains seen { index, direction }

    newQueue =
        unseenNeighbors
        |> List.walk restQueue \accQueue, (index, direction, wayPoints) ->
            updateQueue accQueue { index, direction } (points + wayPoints)

    walkMazeHelper array end newQueue (List.concat seen unseenNeighbors)

IndexDirection : { index : Index2D.Index2D, direction : Direction }
Queue : List { id : IndexDirection, points : U64 }

updateQueue : Queue, IndexDirection, U64 -> Queue
updateQueue = \queue, pos, points ->
    when findFirstIndexWithValue queue \{ id } -> id == pos is
        Ok (index, { points: oldPoints }) ->
            newPoints = Num.min points oldPoints
            List.set queue index { id: pos, points: newPoints }

        Err NotFound ->
            List.append queue { id: pos, points: points }

findFirstIndexWithValue : List a, (a -> Bool) -> Result (U64, a) [NotFound]
findFirstIndexWithValue = \list, fn ->
    helper = \l, index ->
        when l is
            [] -> Err NotFound
            [first, .. as rest] ->
                if fn first then
                    Ok (index, first)
                else
                    helper rest (index + 1)
    helper list 0

getNeighbors = \array, { index, direction } ->
    (left, right) = turn index direction
    when mayGo array index direction is
        Ok path ->
            [path, left, right]

        Err _ ->
            [left, right]

Direction : [East, South, West, North]

turn = \index, direction ->
    (
        (index, direction |> turnRight, 1000),
        (index, direction |> turnLeft, 1000),
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

mayGo = \array, index, direction ->
    nIndex = nextIndex? array index direction
    when Array2D.get array nIndex is
        Ok '#' -> Err Wall
        Err _ -> Err Invalid
        Ok _ ->
            Ok (nIndex, direction, 1)

nextIndex = \array, index, direction ->
    shape = array |> Array2D.shape
    when direction is
        East -> Index2D.incCol index shape
        South -> Index2D.incRow index shape
        West -> Index2D.decCol index shape
        North -> Index2D.decRow index shape

expect
    got = part2 example
    expected = Ok "TODO"
    got == expected

part2 = \_input ->
    Err TODO
