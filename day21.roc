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
        dbg line
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
        dbg (len, number)
        Ok (len * number)

    |> List.sum
    |> Num.toStr
    |> Ok

useNPads = \input, n ->
    dbg ("useNPads", n, List.len input)
    if n == 0 then
        Ok input
        else

    input
    |> List.mapTry? \step ->
        step
        |> findOnPad 'A' [] Directional
    |> List.join
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

# expect
#    got = findOnPad ['0', '2', '9', 'A'] 'A' [] Numeric
#    expected = Ok [['<', 'A', '^', 'A', '>', '^', '^', 'A', 'v', 'v', 'v', 'A']]
#    got == expected

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

    List.concat resultRow resultCol
    |> permutations
    |> Set.fromList
    |> Set.toList
    |> List.dropIf \list -> forbidden from to list pad
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

# expect
#    got = pathNumberic 'A' '0' Numeric
#    expected = Ok ['<', 'A']
#    got == expected

# expect
#    got = pathNumberic '2' '9' Numeric
#    expected = Ok ['>', '^', '^', 'A']
#    got == expected

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

permutations : List a -> List (List a)
permutations = \list ->
    when list is
        [] | [_] ->
            [list]

        [first, .. as rest] ->
            permutations rest
            |> List.joinMap \perm ->
                { start: At 0, end: Before (List.len list) }
                |> List.range
                |> List.map \index ->
                    before = List.takeFirst perm index
                    after = List.dropFirst perm index

                    before
                    |> List.append first
                    |> List.concat after
expect
    got = permutations [1, 2, 3]
    expected = [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
    got == expected

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
