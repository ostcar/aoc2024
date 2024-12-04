app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.7/Tg23npX1TEGNlsYqX1JfrdtvW4OlwLdvsFnJMUJNZSU.tar.br",
}

example =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect
    got = part1 example
    expected = Ok "161"
    got == expected

part1 = \input ->
    input
    |> calcMul
    |> Num.toStr
    |> Ok

calcMul = \input ->
    input
    |> Str.splitOn "mul("
    |> List.map \part ->
        when readMul part is
            Ok num -> num
            Err _ -> 0
    |> List.sum

readMul : Str -> Result U64 _
readMul = \str ->
    (first, str2) = try readNumber str ","
    (second, _) = try readNumber str2 ")"
    Ok (first * second)

readNumber : Str, Str -> Result (U64, Str) _
readNumber = \str, del ->
    { before, after } = try Str.splitFirst str del
    num = try Str.toU64 before
    Ok (num, after)

example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

expect
    got = part2 example2
    expected = Ok "48"
    got == expected

part2 = \input ->
    input
    |> Str.splitOn "do()"
    |> List.map \part ->
        when part |> Str.splitFirst "don't()" is
            Ok { before } ->
                calcMul before

            Err _ ->
                calcMul part
    |> List.sum
    |> Num.toStr
    |> Ok
