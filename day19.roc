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
            |> \n -> n > 0
    |> Num.toStr
    |> Ok

part2 = \input ->
    input
    |> parse?
    |> \{ pattern, designList } ->
        designList
        |> List.map \design ->
            run design pattern (Dict.empty {})
            |> .0
        |> List.sum
    |> Num.toStr
    |> Ok

run = \design, pattern, cache ->
    if design == "" then
        v = 1
        newCache = Dict.insert cache design v
        (v, newCache)
        else

    when Dict.get cache design is
        Ok v -> (v, cache)
        Err KeyNotFound ->
            pattern
            |> List.walk (0, cache) \(n, accCache), p ->
                if Str.startsWith design p then
                    Str.dropPrefix design p
                    |> run pattern accCache
                    |> \(r, newCache) ->
                        (r + n, newCache)
                else
                    (n, accCache)

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

expect
    got = part1 example
    expected = Ok "6"
    got == expected

expect
    got = part2 example
    expected = Ok "16"
    got == expected
