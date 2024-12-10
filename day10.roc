app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D
import array2d.Index2D

example =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """

expect
    got = part1 example
    expected = Ok "36"
    got == expected

part1 = \input ->
    input |> solve Part1

solve = \input, part ->
    map = input |> parse
    startIndexes = findStarts map

    startIndexes
    |> List.walk 0 \acc, start ->
        acc + score map '0' [start] part
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

findStarts = \map ->
    map
    |> Array2D.walk [] { direction: Forwards } \acc, element, index ->
        if element == '0' then
            List.append acc index
        else
            acc

score = \map, current, indexes, part ->
    if current == '9' then
        when part is
            Part1 ->
                indexes
                |> Set.fromList
                |> Set.len

            Part2 ->
                indexes
                |> List.len
        else

    next = indexes |> List.joinMap \index -> neighbors map index (current + 1)
    score map (current + 1) next part

neighbors : Array2D.Array2D U8, Index2D.Index2D, U8 -> List Index2D.Index2D
neighbors = \map, index, value ->
    shape = Array2D.shape map
    [
        decRow index shape,
        incCol index shape,
        incRow index shape,
        decCol index shape,
    ]
    |> List.keepOks \nIndex ->
        nIndex
        |> Result.try \i ->
            if Array2D.get map i == Ok value then
                Ok i
            else
                Err NoIDontWantYou

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
    expected = Ok "81"
    got == expected

part2 = \input ->
    input |> solve Part2
