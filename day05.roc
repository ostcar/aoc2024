app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

example =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """

expect
    got = part1 example
    expected = Ok "143"
    got == expected

part1 = \input ->
    { rules, updates } = input |> parse?

    updates
    |> List.keepIf \update -> checkUpdate rules update
    |> List.mapTry? findMid
    |> List.sum
    |> Num.toStr
    |> Ok

parse = \input ->
    when input |> Str.splitOn "\n\n" is
        [rawRules, rawUpdates] ->
            rules = rawRules |> parseRules?
            updates = rawUpdates |> parseUpdates?
            Ok { rules, updates }

        _ -> Err InvalidInput

parseRules = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        when line |> Str.splitOn "|" is
            [before, after] ->
                b = before |> Str.toU64?
                a = after |> Str.toU64?
                Ok (b, a)

            _ -> Err InvalidRule

parseUpdates = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        line
        |> Str.splitOn ","
        |> List.mapTry \rawNum ->
            rawNum |> Str.toU64

checkUpdate = \rules, update ->
    when update is
        [] | [_] -> Bool.true
        [first, .. as rest] ->
            correct =
                rules
                |> List.all \(before, after) ->
                    if first != after then
                        Bool.true
                        else

                    rest |> List.contains before |> Bool.not

            if correct then
                checkUpdate rules rest
            else
                Bool.false

findMid = \list ->
    mid = (list |> List.len) // 2
    list |> List.get mid

expect
    got = part2 example
    expected = Ok "123"
    got == expected

part2 = \input ->
    { rules, updates } = input |> parse?

    updates
    |> List.dropIf \update -> checkUpdate rules update
    |> List.map \update -> orderUpdate rules update
    |> List.mapTry? findMid
    |> List.sum
    |> Num.toStr
    |> Ok

orderUpdate = \rules, update ->
    update
    |> List.sortWith \a, b ->
        rules
        |> List.walkUntil EQ \_, (before, after) ->
            if a == before && b == after then
                Break LT
            else if a == after && b == before then
                Break GT
            else
                Continue EQ
