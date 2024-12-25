app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

part1 = \input ->
    input
    |> parse?
    |> \{ pattern, designList } ->
        designList
        |> List.countIf \design ->
            run design pattern (Dict.empty {})
            |> .0
    |> Num.toStr
    |> Ok

run = \design, pattern, cache ->
    if design == "" then
        v = Bool.true
        newCache = Dict.insert cache design v
        (v, newCache)
        else

    when Dict.get cache design is
        Ok v -> (v, cache)
        Err KeyNotFound ->
            pattern
            |> List.walkUntil (Bool.true, cache) \(_, accCache), p ->
                if Str.startsWith design p then
                    Str.dropPrefix design p
                    |> run pattern accCache
                    |> \(r, newCache) ->
                        if r then
                            Break (Bool.true, newCache)
                        else
                            Continue (Bool.false, newCache)
                else
                    Continue (Bool.false, accCache)

            |> \(v, tCache) ->
                newCache = Dict.insert tCache design v
                (v, newCache)

parse : Str -> Result { pattern : List Str, designList : List Str } _
parse = \input ->
    { before, after } = input |> Str.splitFirst? "\n\n"

    pattern =
        before
        |> Str.splitOn ", "

    designList =
        after
        |> Str.splitOn "\n"

    Ok { pattern, designList }

part2 = \_input ->
    Err TODO

expect
    example =
        """
        r, wr, b, g, bwu, rb, gb, br

        brwrr
        bggr
        gbbr
        rrbgbr
        ubwu
        bwurrg
        brgr
        bbrgwb
        """
    got = part1 example
    expected = Ok "6"
    got == expected
