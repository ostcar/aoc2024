app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

example =
    "125 17"

expect
    got = part1 example
    expected = Ok "55312"
    got == expected

part1 = \input ->
    input
    |> parse?
    |> blink 25
    |> List.len
    |> Num.toStr
    |> Ok

parse = \input ->
    input
    |> Str.splitOn " "
    |> List.mapTry Str.toU64

blink = \numbers, times ->
    if times == 0 then
        numbers
        else

    numbers
    |> List.joinMap \e ->
        if e == 0 then
            [1]
            else

        digiCount = numLen e 0
        if digiCount % 2 == 0 then
            p = 10 |> Num.powInt (digiCount // 2)
            [e // p, e % p]
        else
            [e * 2024]
    |> blink (times - 1)

numLen = \a, result ->
    # This should be implemented with Num.log10 if it would exist
    if a // 10 == 0 then
        result + 1
    else
        numLen (a // 10) (result + 1)

part2 = \input ->
    input
    |> parse?
    |> List.map \e -> blink2 e 75 (Dict.empty {}) |> \(v, _) -> v
    |> List.sum
    |> Num.toStr
    |> Ok

# This does the same as blink. But faster and with a cache
blink2 : U64, U64, Cache -> (U64, Cache)
blink2 = \number, times, cache ->
    if times == 0 then
        (1, cache)
    else if number == 0 then
        blinkCache 1 (times - 1) cache
    else
        digiCount = numLen number 0
        if digiCount % 2 == 0 then
            p = 10 |> Num.powInt (digiCount // 2)
            (front, cache2) = blinkCache (number // p) (times - 1) cache
            (back, cache3) = blinkCache (number % p) (times - 1) cache2
            (front + back, cache3)
        else
            blinkCache (number * 2024) (times - 1) cache

Cache : Dict (U64, U64) U64

blinkCache : U64, U64, Cache -> (U64, Cache)
blinkCache = \number, times, cache ->
    when Dict.get cache (number, times) is
        Ok v -> (v, cache)
        Err _ ->
            (v, cache2) = blink2 number times cache
            cache3 = Dict.insert cache2 (number, times) v
            (v, cache3)
