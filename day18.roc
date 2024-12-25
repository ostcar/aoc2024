app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

part1 = \input ->
    solvePart1 input 70 1024

solvePart1 = \input, size, seconds ->
    blocks = input |> parse?

    run? blocks size seconds
    |> Num.toStr
    |> Ok

run = \blocks, size, seconds ->
    someBlocks = blocks |> List.takeFirst seconds

    isGoal = \{ col, row } ->
        col == size && row == size

    getNeighbors = \pos ->
        pos
        |> around size
        |> List.dropIf \{ node: p } -> List.contains someBlocks p

    shortestPath { col: 0, row: 0 } isGoal getNeighbors

around = \{ col, row }, size ->
    top = col |> Num.subChecked 1 |> Result.map \v -> { node: { col: v, row }, distance: 1 }
    left = row |> Num.subChecked 1 |> Result.map \v -> { node: { row: v, col }, distance: 1 }
    bottom = if row >= size then Err OutOfBounds else Ok { node: { row: row + 1, col }, distance: 1 }
    right = if col >= size then Err OutOfBounds else Ok { node: { col: col + 1, row }, distance: 1 }

    [top, left, bottom, right]
    |> List.keepOks \v -> v

part2 = \input ->
    solvePart2 input 70

solvePart2 : Str, U64 -> Result Str _
solvePart2 = \input, size ->
    blocks = input |> parse?

    seconds = solvePart2Helper blocks size 1

    List.get? blocks (seconds - 1)
    |> \{ col, row } -> "$(Num.toStr row),$(Num.toStr col)"
    |> Ok

solvePart2Helper = \blocks, size, seconds ->
    when run blocks size seconds is
        Ok _ ->
            dbg seconds
            solvePart2Helper blocks size (seconds + 1)

        Err NotFound ->
            seconds

parse = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        when Str.splitOn line "," is
            [before, after] -> Ok { row: Str.toU64? before, col: Str.toU64? after }
            _ -> Err InvalidInput

Node a : a where a implements Eq
NodeDistance a : { distance : U64, node : Node a }
GetNeighbors a : Node a -> List (NodeDistance a)
IsGoal a : Node a -> Bool

shortestPath : Node a, IsGoal a, GetNeighbors a -> Result U64 [NotFound]
shortestPath = \start, isGoal, getNeighbors ->
    shortestPathHelper [{ distance: 0, node: start }] [] isGoal getNeighbors

# shortestPathHelper : List (NodeDistance a), List (Node a), IsGoal a, GetNeighbors a -> Result U64 [NotFound]
shortestPathHelper = \list, seen, isGoal, getNeighbors ->
    when list is
        [] -> Err NotFound
        [{ distance, node }, .. as rest] ->
            if isGoal node then
                Ok distance
                else

            newSeen = seen |> List.append node

            node
            |> getNeighbors
            |> List.dropIf \neighbor -> List.contains newSeen neighbor.node
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

example =
    """
    5,4
    4,2
    4,5
    3,0
    2,1
    6,3
    2,4
    1,5
    0,6
    3,3
    2,6
    5,1
    1,2
    5,5
    2,5
    6,5
    1,4
    0,4
    6,4
    1,1
    6,1
    1,0
    0,5
    1,6
    2,0
    """

expect
    got = solvePart1 example 6 12
    expected = Ok "22"
    got == expected

expect
    got = solvePart2 example 6
    expected = Ok "6,1"
    got == expected
