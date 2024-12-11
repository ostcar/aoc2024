app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
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
    |> blink 75
    |> List.len
    |> Num.toStr
    |> Ok
