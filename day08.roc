app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

example =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """

expect
    got = part1 example
    expected = Ok "14"
    got == expected

part1 = \input ->
    { map, maxCol, maxRow } = parse input

    map
    |> Dict.walk [] \acc, _, possitions ->
        addPairs possitions acc
    |> List.walk [] \acc, (pos1, pos2) ->
        calcAntinode acc pos1 pos2
    |> List.keepIf \pos -> onMap pos maxCol maxRow
    |> countUnique
    |> Num.toStr
    |> Ok

Position : { col : I64, row : I64 }
Map : Dict U8 (List Position)

parse : Str -> { map : Map, maxRow : I64, maxCol : I64 }
parse = \input ->
    map =
        input
        |> Str.splitOn "\n"
        |> List.walkWithIndex (Dict.empty {}) \acc, line, row ->
            line
            |> Str.toUtf8
            |> List.walkWithIndex acc \acc2, char, col ->
                if char == '.' then
                    acc2
                    else

                acc2
                |> Dict.update char \value ->
                    value
                    |> Result.withDefault []
                    |> List.append { row: row |> Num.toI64, col: col |> Num.toI64 }
                    |> Ok
    maxCol =
        input
        |> Str.toUtf8
        |> List.findFirstIndex \c -> c == '\n'
        |> Result.withDefault 0
        |> Num.sub 1
        |> Num.toI64

    maxRow =
        input
        |> Str.toUtf8
        |> List.countIf \c -> c == '\n'
        |> Num.toI64

    { map, maxCol, maxRow }

addPairs = \list, result ->
    when list is
        [] | [_] -> result
        [a, .. as rest] ->
            newResult =
                rest
                |> List.walk result \acc, b ->
                    List.append acc (a, b)
            addPairs rest newResult

calcAntinode = \list, { col: colA, row: rowA }, { col: colB, row: rowB } ->
    list
    |> List.append { col: colA + colA - colB, row: rowA + rowA - rowB }
    |> List.append { col: colB + colB - colA, row: rowB + rowB - rowA }

expect
    a1 = { col: 4, row: 3 }
    a2 = { col: 5, row: 5 }
    got = calcAntinode [] a1 a2
    expected = [{ col: 3, row: 1 }, { col: 6, row: 7 }]
    got == expected

onMap = \{ row, col }, maxCol, maxRow ->
    row >= 0 && row <= maxRow && col >= 0 && col <= maxCol

countUnique = \list ->
    list
    |> Set.fromList
    |> Set.len

expect
    got = part2 example
    expected = Ok "34"
    got == expected

part2 = \input ->
    { map, maxCol, maxRow } = parse input

    map
    |> Dict.walk [] \acc, _, possitions ->
        addPairs possitions acc
    |> List.walk [] \acc, pair ->
        walkOnMap maxCol maxRow pair acc
    |> countUnique
    |> Num.toStr
    |> Ok

walkOnMap = \maxCol, maxRow, ({ col: colA, row: rowA }, { col: colB, row: rowB }), result ->
    colDiff = colA - colB
    rowDiff = rowA - rowB

    result
    |> List.append { col: colA, row: rowA }
    |> addPosStep maxCol maxRow { col: colA, row: rowA } colDiff rowDiff
    |> addPosStep maxCol maxRow { col: colA, row: rowA } -colDiff -rowDiff

addPosStep = \result, maxCol, maxRow, { col, row }, colStep, rowStep ->
    newPos = { col: col + colStep, row: row + rowStep }
    if onMap newPos maxCol maxRow then
        newResult = result |> List.append newPos
        addPosStep newResult maxCol maxRow newPos colStep rowStep
    else
        result
