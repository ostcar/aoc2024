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
        (len, _) =
            line
            |> Str.toUtf8
            |> findOnPad? 'A' [] Numeric
            |> useNPads? directionalPads
            |> List.walk (Num.maxU64, []) \(acc, old), solution ->
                if List.len solution < acc then
                    (List.len solution, solution)
                else
                    (acc, old)

        number =
            line
            |> Str.dropSuffix "A"
            |> Str.toU64?
        Ok (len * number)

    |> List.sum
    |> Num.toStr
    |> Ok

useNPads = \input, n ->
    if n == 0 then
        Ok input
        else

    input
    |> List.mapTry? \step ->
        step
        |> findOnPad 'A' [] Directional
    |> List.join
    |> List.walk (Num.maxU64, []) \(acc, old), solution ->
        len = List.len solution
        if len > acc |> Num.addSaturated 1 then
            (acc, old)
        else if len < acc |> Num.subSaturated 1 then
            (len, [solution])
        else
            (len, List.append old solution)
    |> .1
    |> useNPads (n - 1)

findOnPad = \input, cur, result, pad ->
    when input is
        [] -> Ok result
        [first, .. as rest] ->
            pathList = pathOnPad? cur first pad
            nResult =
                if result == [] then
                    pathList
                else
                    List.joinMap result \oneResult ->
                        pathList
                        |> List.map \onePath ->
                            List.concat oneResult onePath

            findOnPad rest first nResult pad

pathOnPad = \from, to, pad ->
    (getFromCol, getFromRow) =
        when pad is
            Numeric -> (numericCol, numericRow)
            Directional -> (directionalCol, directionalRow)

    fromCol = getFromCol? from
    toCol = getFromCol? to
    resultCol =
        when Num.compare fromCol toCol is
            GT -> List.repeat '<' (fromCol - toCol)
            LT -> List.repeat '>' (toCol - fromCol)
            EQ -> []

    fromRow = getFromRow? from
    toRow = getFromRow? to
    resultRow =
        when Num.compare fromRow toRow is
            GT -> List.repeat '^' (fromRow - toRow)
            LT -> List.repeat 'v' (toRow - fromRow)
            EQ -> []

    var1 = List.concat resultRow resultCol
    var2 = List.concat resultCol resultRow
    both =
        if var1 == var2 || forbidden from to var2 pad then
            [var1]
        else if forbidden from to var1 pad then
            [var2]
        else
            [var1, var2]

    both
    |> List.map \list -> list |> List.append 'A'
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
