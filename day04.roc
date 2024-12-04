app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.7/Tg23npX1TEGNlsYqX1JfrdtvW4OlwLdvsFnJMUJNZSU.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D
import array2d.Index2D

example =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect
    got = part1 example
    expected = Ok "18"
    got == expected

part1 = \input ->
    input
    |> parse
    |> findXMAS
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

findXMAS = \array ->
    array
    |> Array2D.walk 0 { direction: Forwards } \acc, _, index ->
        acc + countXMASAtIndex array index

countXMASAtIndex = \array, index ->
    if Array2D.get array index != Ok 'X' then
        0
        else

    goAllDirections array index 3
    |> List.countIf \list -> list == ['M', 'A', 'S']

goAllDirections = \array, index, steps ->
    [
        goDirection array index PrevRow PrevCol steps [],
        goDirection array index SameRow PrevCol steps [],
        goDirection array index NextRow PrevCol steps [],
        goDirection array index PrevRow SameCol steps [],
        goDirection array index NextRow SameCol steps [],
        goDirection array index PrevRow NextCol steps [],
        goDirection array index SameRow NextCol steps [],
        goDirection array index NextRow NextCol steps [],
    ]
    |> List.keepOks \r -> r

goDirection = \array, index, row, col, steps, result ->
    if steps == 0 then
        Ok result
        else

    nextIndex = try Index2D.adjacentTo index (Array2D.shape array) row col
    nextValue = try Array2D.get array nextIndex
    nextResult = result |> List.append nextValue
    goDirection array nextIndex row col (steps - 1) nextResult

expect
    got = part2 example
    expected = Ok "9"
    got == expected

part2 = \input ->
    input
    |> parse
    |> arrayCountIndex isXMAS
    |> Num.toStr
    |> Ok

arrayCountIndex = \array, fn ->
    Array2D.walk array 0 { direction: Forwards } \acc, _, index ->
        if fn array index then
            acc + 1
        else
            acc

isXMAS = \array, index ->
    if Array2D.get array index != Ok 'A' then
        Bool.false
        else

    getDiagonals array index
    |> Result.map \(d1a, d1b, d2a, d2b) ->
        (d1a == 'M' && d1b == 'S' || d1a == 'S' && d1b == 'M') && (d2a == 'M' && d2b == 'S' || d2a == 'S' && d2b == 'M')
    |> Result.withDefault Bool.false

getDiagonals = \array, index ->
    d1a = try getAtDirection array index PrevRow PrevCol
    d1b = try getAtDirection array index NextRow NextCol
    d2a = try getAtDirection array index PrevRow NextCol
    d2b = try getAtDirection array index NextRow PrevCol
    Ok (d1a, d1b, d2a, d2b)

getAtDirection = \array, index, row, col ->
    Index2D.adjacentTo index (Array2D.shape array) row col
    |> Result.try \atIndex -> Array2D.get array atIndex
