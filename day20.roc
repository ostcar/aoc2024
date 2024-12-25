app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
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
    |> List.countIf \(nPos, distance) ->
        path
        |> List.dropFirst (startIndex + 100 + distance)
        |> List.contains nPos

moveN : Position, U64 -> List (Position, U64)
moveN = \{ col, row }, n ->
    { start: At (col |> Num.subSaturated n), end: At (col + n) }
    |> List.range
    |> List.walk [] \acc, newCol ->
        { start: At (row |> Num.subSaturated n), end: At (row + n) }
        |> List.range
        |> List.walk acc \acc2, newRow ->
            distance = Num.absDiff row newRow + Num.absDiff col newCol
            if distance > n || distance <= 1 then
                acc2
            else
                List.append acc2 ({ row: newRow, col: newCol }, distance)

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
