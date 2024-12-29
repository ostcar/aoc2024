app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

part1 = \input ->
    input
    |> parse?
    |> solve
    |> Num.toStr
    |> Ok

part2 = \input ->
    input
    |> parse?
    |> Dict.map \_, v ->
        v |> Set.fromList
    |> solvePart2
    |> Str.joinWith ","
    |> Ok

Computer : Str

solve = \dict ->
    dict
    |> Dict.walk 0 \acc, c1, list ->
        list
        |> listWalkOnTwo acc \counter, c2, c3 ->
            dict
            |> Dict.get c2
            |> Result.withDefault []
            |> List.contains c3
            |> \isTripple ->
                if isTripple then
                    if oneStartsWithT c1 c2 c3 then
                        counter + 1
                    else
                        counter
                else
                    counter

solvePart2 = \dict ->
    dict
    |> Dict.walk [] \acc, c1, set ->
        findBiggest dict set [c1]
        |> biggerList acc

findBiggest = \dict, set, result ->
    set
    |> Set.walk result \acc, elem ->
        when dict |> Dict.get elem is
            Ok otherSet ->
                findBiggest dict (set |> Set.intersection otherSet) (List.append result elem)
                |> biggerList acc

            Err KeyNotFound ->
                List.append result elem
                |> biggerList acc

biggerList = \l1, l2 ->
    if List.len l1 > List.len l2 then
        l1
    else
        l2

oneStartsWithT = \c1, c2, c3 ->
    Str.startsWith c1 "t" || Str.startsWith c2 "t" || Str.startsWith c3 "t"

parse : Str -> Result (Dict Computer (List Computer)) _
parse = \input ->
    input
    |> Str.splitOn "\n"
    |> List.walkTry? (Dict.empty {}) \dict, line ->
        when line |> Str.splitOn "-" is
            [first, second] ->
                when (first |> Str.toUtf8, second |> Str.toUtf8) is
                    ([a1, a2], [b1, b2]) ->
                        (a, b) =
                            when (Num.compare a1 b1, Num.compare a2 b2) is
                                (GT, _) | (EQ, GT) ->
                                    (second, first)

                                (LT, _) | (EQ, LT) ->
                                    (first, second)

                                _ ->
                                    return Err (BothSidesAreTheSame a1 a2)
                        dict
                        |> Dict.update a \r ->
                            when r is
                                Ok l -> l |> List.append b |> Ok
                                Err Missing -> Ok [b]
                        |> Ok

                    _ -> Err InvalidInput

            _ -> Err InvalidInput

    |> Dict.map \_, list ->
        list |> List.sortWith compareComputer

    |> Ok

compareComputer = \c1, c2 ->
    when (c1 |> Str.toUtf8, c2 |> Str.toUtf8) is
        ([a1, a2], [b1, b2]) ->
            when (Num.compare a1 b1, Num.compare a2 b2) is
                (GT, _) | (EQ, GT) ->
                    GT

                (LT, _) | (EQ, LT) ->
                    LT

                _ ->
                    EQ

        _ ->
            EQ

listWalkOnTwo : List a, b, (b, a, a -> b) -> b
listWalkOnTwo = \list, acc, fn ->
    when list is
        [] | [_] -> acc
        [first, .. as rest] ->
            newAcc =
                rest
                |> List.walk acc \acc2, second ->
                    fn acc2 first second
            listWalkOnTwo (rest) newAcc fn

example =
    """
    kh-tc
    qp-kh
    de-cg
    ka-co
    yn-aq
    qp-ub
    cg-tb
    vc-aq
    tb-ka
    wh-tc
    yn-cg
    kh-ub
    ta-co
    de-co
    tc-td
    tb-wq
    wh-td
    ta-ka
    td-qp
    aq-cg
    wq-ub
    ub-vc
    de-ta
    wq-aq
    wq-vc
    wh-yn
    ka-de
    kh-ta
    co-tc
    wh-qp
    tb-vc
    td-yn
    """
expect
    got = part1 example
    expected = Ok "7"
    got == expected

expect
    got = part2 example
    expected = Ok "co,de,ka,ta"
    got == expected
