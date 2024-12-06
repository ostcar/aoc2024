app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D
import array2d.Index2D

example =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

expect
    got = part1 example
    expected = Ok "41"
    got == expected

part1 = \input ->
    map = input |> parse
    start = findStart? map

    go? map start Up (List.withCapacity 40)
    |> List.len
    |> Num.add 1
    |> Num.toStr
    |> Ok

parse = \input ->
    input
    |> Str.trim
    |> Str.splitOn "\n"
    |> List.map \line ->
        line
        |> Str.toUtf8
    |> Array2D.fromLists FitShortest

findStart = \map ->
    map |> Array2D.findFirstIndex \e -> e == '^'

go = \map, current, direction, indexes ->
    if List.contains indexes (direction, current) then
        Err Loop
        else

    newIndexes = listAppendUnique indexes (direction, current)
    when inFrontIndex map current direction is
        Err OutOfBounds ->
            indexes
            |> List.map \(_, index) -> index
            |> Set.fromList
            |> Set.toList
            |> Ok

        Ok index ->
            when Array2D.get? map index is
                '#' ->
                    go map current (turn direction) newIndexes

                '.' | '^' ->
                    go map index direction newIndexes

                _ ->
                    Err InvalidInput

listAppendUnique = \list, element ->
    if List.contains list element then
        list
    else
        List.append list element

turn = \direction ->
    when direction is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up

inFrontIndex = \map, current, direction ->
    fn =
        when direction is
            Up -> decRow
            Right -> incCol
            Down -> incRow
            Left -> decCol

    fn current (Array2D.shape map)

decRow = \index, _shape ->
    if Index2D.isColStart index then
        Err OutOfBounds
    else
        Ok { index & row: index.row - 1 }

incRow = \index, shape ->
    if Index2D.isColEnd index shape then
        Err OutOfBounds
    else
        Ok { index & row: index.row + 1 }

decCol = \index, _shape ->
    if Index2D.isRowStart index then
        Err OutOfBounds
    else
        Ok { index & col: index.col - 1 }

incCol = \index, shape ->
    if Index2D.isRowEnd index shape then
        Err OutOfBounds
    else
        Ok { index & col: index.col + 1 }

expect
    got = part2 example
    expected = Ok "6"
    got == expected

part2 = \input ->
    # Err TODO
    map = input |> parse
    start = findStart? map

    go map start Up []
    |> try
    |> List.walk 0 \acc, index ->
        if index == start then
            acc
            else

        changedMap = Array2D.set map index '#'
        when go changedMap start Up [] is
            Err Loop -> acc + 1
            _ -> acc
    |> Num.toStr
    |> Ok
