app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

part1 = \input ->
    path = input |> parse?

    path
    |> List.walkWithIndex 0 \acc, pos, startIndex ->
        acc + cheatPlaces pos startIndex path 2

    |> Num.toStr
    |> Ok

part2 = \input ->
    path = input |> parse?

    path
    |> List.walkWithIndex 0 \acc, pos, startIndex ->
        acc + cheatPlaces pos startIndex path 20

    |> Num.toStr
    |> Ok

cheatPlaces : Position, U64, List Position, U64 -> U64
cheatPlaces = \pos, startIndex, path, cheatSteps ->
    moveN pos cheatSteps
    |> List.keepIf \nPos ->
        path
        |> List.dropFirst (startIndex + 102)
        |> List.contains nPos
    |> List.len

moveN : Position, U64 -> List Position
moveN = \{ col, row }, n ->
    { start: At 0, end: Before n }
    |> List.range
    |> List.joinMap \a ->
        b = n - a
        [
            Result.map (row |> Num.subChecked a) \newRow -> { row: newRow, col: col + b },
            { col: col + a, row: row + b } |> Ok,
            Result.map (col |> Num.subChecked b) \newCol -> { row: row + a, col: newCol },
            Result.map2 (col |> Num.subChecked a) (row |> Num.subChecked b) \newCol, newRow -> { col: newCol, row: newRow },
        ]
        |> List.keepOks \v -> v

expect
    col = 1
    row = 1
    got = moveN { col, row } 2
    expected =
        [
            Ok { col: col + 2, row: row },
            Ok { col: col, row: row + 2 },
            col |> Num.subChecked 2 |> \r -> Result.map r \newCol -> { col: newCol, row: row },
            row |> Num.subChecked 2 |> \r -> Result.map r \newRow -> { col: col, row: newRow },
            Ok { col: col + 1, row: row - 1 },
            Ok { col: col + 1, row: row + 1 },
            Ok { col: col - 1, row: row + 1 },
            Ok { col: col - 1, row: row - 1 },
        ]
        |> List.keepOks \v -> v
    got == expected

Position : { col : U64, row : U64 }

parse : Str -> Result (List Position) _
parse = \input ->
    width = input |> Str.toUtf8 |> List.findFirstIndex? \c -> c == '\n'
    data = input |> Str.toUtf8 |> List.dropIf \c -> c == '\n'
    start = data |> List.findFirstIndex? \e -> e == 'S'

    path = goPath data width 0 start [indexToPos start width]

    Ok path

goPath = \data, width, beforeIndex, index, result ->
    when findNextIndex data beforeIndex index width is
        Err _ -> result
        Ok nextIndex ->
            goPath data width index nextIndex (List.append result (indexToPos nextIndex width))

findNextIndex = \data, beforeIndex, index, width ->
    [
        Ok (index + 1),
        Ok (index - 1),
        index |> Num.subChecked width,
        Ok (index + width),
    ]
    |> List.keepOks \e ->
        Result.try e \i ->
            if i == beforeIndex then
                Err NoBackwards
                else

            v = List.get? data i
            if v == '#' then
                Err Blocked
            else
                Ok i
    |> List.first

indexToPos = \index, width ->
    col = index % width
    row = index // width
    { col, row }
