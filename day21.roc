app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

part1 = \input ->
    solve input 2

part2 = \input ->
    solve input 25

solve = \input, directionalPads ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry? \line ->
        solution =
            line
            |> Str.toUtf8
            |> \sequence -> Dict.single sequence 1
            |> solveSequencesInDict? Numeric
            |> useNDirectionalPads? directionalPads

        len =
            solution
            |> Dict.walk 0 \acc, sequence, num ->
                acc + (List.len sequence) * num

        number =
            line
            |> Str.dropSuffix "A"
            |> Str.toU64?
        Ok (len * number)

    |> List.sum
    |> Num.toStr
    |> Ok

useNDirectionalPads = \input, n ->
    if n == 0 then
        Ok input
        else

    input
    |> solveSequencesInDict? Directional
    |> useNDirectionalPads (n - 1)

solveSequencesInDict : Dict (List U8) U64, [Numeric, Directional] -> Result (Dict (List U8) U64) _
solveSequencesInDict = \dict, pad ->
    dict
    |> Dict.toList # There is no Dict.walkTry
    |> List.walkTry (Dict.empty {}) \acc, (oldSequence, num) ->
        solveOneSequence? oldSequence 'A' [] pad
        |> List.walk acc \newDict, newSequence ->
            newDict
            |> Dict.update newSequence \r ->
                when r is
                    Err Missing -> Ok num
                    Ok n -> Ok (n + num)
        |> Ok

solveOneSequence : List U8, U8, List (List U8), [Numeric, Directional] -> Result (List (List U8)) _
solveOneSequence = \input, cur, result, pad ->
    when input is
        [] -> Ok result
        [first, .. as rest] ->
            path = pathOnPad? cur first pad
            newResult =
                if result == [] then
                    [path]
                else
                    List.append result path

            solveOneSequence rest first newResult pad

pathOnPad = \from, to, pad ->
    (getFromCol, getFromRow) =
        when pad is
            Numeric -> (numericCol, numericRow)
            Directional -> (directionalCol, directionalRow)

    fromRow = getFromRow? from
    toRow = getFromRow? to
    resultRow =
        when Num.compare fromRow toRow is
            GT -> List.repeat '^' (fromRow - toRow)
            LT -> List.repeat 'v' (toRow - fromRow)
            EQ -> []

    fromCol = getFromCol? from
    toCol = getFromCol? to
    cmpLeftRight = Num.compare fromCol toCol
    resultCol =
        when cmpLeftRight is
            GT -> List.repeat '<' (fromCol - toCol)
            LT -> List.repeat '>' (toCol - fromCol)
            EQ -> []

    var1 = List.concat resultRow resultCol
    var2 = List.concat resultCol resultRow
    combined =
        if var1 == var2 || forbidden from to var2 pad || (cmpLeftRight == LT && (forbidden from to var1 pad |> Bool.not)) then
            var1
        else
            var2

    combined
    |> List.append 'A'
    |> Ok

forbidden = \from, to, list, pad ->
    when pad is
        Directional ->
            if to == '<' && List.last list == Ok 'v' then
                Bool.true
            else if from == '<' && List.first list == Ok '^' then
                Bool.true
            else
                Bool.false

        Numeric ->
            if to == '0' && List.last list == Ok '>' then
                Bool.true
            else if to == 'A' && List.last list == Ok '>' && List.dropLast list 1 |> List.last == Ok '>' then
                Bool.true
            else if from == '0' && List.first list == Ok '<' then
                Bool.true
            else if from == 'A' && List.first list == Ok '<' && List.dropFirst list 1 |> List.first == Ok '<' then
                Bool.true
            else
                Bool.false

numericRow = \n ->
    when n is
        '7' | '8' | '9' -> 0 |> Ok
        '4' | '5' | '6' -> 1 |> Ok
        '1' | '2' | '3' -> 2 |> Ok
        '0' | 'A' -> 3 |> Ok
        _ -> Err InvalidInpit

numericCol = \n ->
    when n is
        '1' | '4' | '7' -> 0 |> Ok
        '0' | '2' | '5' | '8' -> 1 |> Ok
        'A' | '3' | '6' | '9' -> 2 |> Ok
        _ -> Err InvalidInpit

directionalRow = \n ->
    when n is
        '^' | 'A' -> 0 |> Ok
        '<' | 'v' | '>' -> 1 |> Ok
        _ -> Err InvalidInpit

directionalCol = \n ->
    when n is
        '<' -> 0 |> Ok
        '^' | 'v' -> 1 |> Ok
        'A' | '>' -> 2 |> Ok
        _ -> Err InvalidInpit

expect
    example =
        """
        029A
        980A
        179A
        456A
        379A
        """
    got = part1 example
    expected = Ok "126384"
    got == expected
