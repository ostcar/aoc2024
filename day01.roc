app [part1, part2] {
    pf: platform "../roc-aoc-platform/platform/main.roc",
}

exampleInput =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

expect
    got = part1 exampleInput
    expected = Ok "11"
    got == expected

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> parseNumbers
    |> try
    |> sortNumbers
    |> diffNumbers
    |> List.sum
    |> Num.toStr
    |> Ok

parseNumbers = \input ->
    input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.mapTry \line ->
            when Str.splitOn line "   " is
                [s1, s2] ->
                    n1 = Str.toU64? s1
                    n2 = Str.toU64? s2
                    Ok (n1, n2)

                _ -> Err InvalidInput
        |> try
        |> List.walk ([], []) \(l1, l2), (n1, n2) ->
            (List.append l1 n1, List.append l2 n2)
        |> Ok

sortNumbers = \(list1, list2) ->
    (List.sortAsc list1, List.sortAsc list2)

diffNumbers = \(list1, list2) ->
    List.map2 list1 list2 Num.absDiff

expect
    got = part2 exampleInput
    expected = Ok "31"
    got == expected

part2 = \input ->
    input
    |> parseNumbers
    |> try
    |> \(list1, list2) ->
        list1
        |> List.walk 0 \acc, n ->
            inList2 = list2 |> List.countIf \e -> e == n
            acc + n * inList2
    |> Num.toStr
    |> Ok
