app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

examplePart =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

expect part1 examplePart == Ok "2"

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> parse
    |> try
    |> List.countIf isSave
    |> Num.toStr
    |> Ok

parse = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        line
        |> Str.splitOn " "
        |> List.mapTry \n ->
            n
            |> Str.toU64

isSave = \report ->
    when report is
        [] -> Bool.false
        [_] -> Bool.true
        [first, second, ..] ->
            asc = first < second

            report
            |> List.dropFirst 1
            |> List.walkTry first \before, current ->
                if before == current then
                    Err Unequal
                else if asc && before > current || !asc && before < current then
                    Err Unordered
                else if Num.absDiff before current > 3 then
                    Err BigDiffer
                else
                    Ok current
            |> Result.isOk

expect
    got = part2 examplePart
    expected = Ok "4"
    got == expected

part2 = \input ->
    input
    |> parse
    |> try
    |> List.countIf \report -> withTolerance report isSave
    |> Num.toStr
    |> Ok

withTolerance = \list, func ->
    if func list then
        Bool.true
        else

    { start: At 0, end: Before (List.len list) }
    |> List.range
    |> List.any \index ->
        list
        |> List.dropAt index
        |> func
