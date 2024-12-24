app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

# Check your input. There are the letters w, u, b, r and g. If pattern should contain 4 of them as a single pattern. Look for ' w,'
# If all combos with this letter exist (rw, ru, rb, rr and rg), then this solution works.
# If all other reversed combos exist (wr, ur, br, rr and gr), then you have to change the solution from `List.endsWith` to `List.startsWith`
myLetter = 'r'

part1 = \input ->
    input
    |> parse?
    |> \{ pattern, designList } ->
        designList
        |> List.countIf \design ->
            if List.last design != Ok myLetter then
                Bool.true
                else

            pattern
            |> List.any \p ->
                List.endsWith design p
    |> Num.toStr
    |> Ok

parse : Str -> Result { pattern : List (List U8), designList : List (List U8) } _
parse = \input ->
    { before, after } = input |> Str.splitFirst? "\n\n"

    pattern =
        before
        |> Str.splitOn ", "
        |> List.map Str.toUtf8

    designList =
        after
        |> Str.splitOn "\n"
        |> List.map Str.toUtf8

    Ok { pattern, designList }

part2 = \input ->
    input
    |> parse?
    |> \{ pattern, designList } ->
        dbg pattern
        designList
        |> List.walk 0 \acc, design ->
            v = splitCount design pattern
            dbg (design |> Str.fromUtf8 |> Result.withDefault "xxx")
            dbg v
            acc |> Num.add v
    |> Num.toStr
    |> Ok

splitCount = \list, pattern ->
    if List.isEmpty list then
        []
        else

    { start: At 1, end: Before (List.len list) }
    |> List.range
    |> List.map \index ->
        { before, others } = list |> List.splitAt index
        v1 = splitCount before pattern
        v2 = splitCount others pattern
        combine v1 v2

    |> \n -> if List.contains pattern list then List.append n list else n


combine = \a, b, result ->
    if List.isEmpty b then
        []
        else

    when a is
        [] -> []
        [one] ->
            b
            |> List.map \v -> b |> List.append v
            |> List.concat result

        [first, ..as rest] ->


expect
    example =
        """
        r, wr, b, g, bwu, rb, gb, br

        rrbgbr
        """
    got = part2 example
    expected = Ok "16"
    got == expected
