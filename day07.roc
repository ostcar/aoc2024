app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser, sepBy, const, keep, skip]
import parser.String exposing [parseStr, string, digits]

example =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """

expect
    got = part1 example
    expected = Ok "3749"
    got == expected

part1 = \input ->
    solve input Part1

solve = \input, part ->
    equations = parseStr? puzzleParser (input |> Str.trim)

    equations
    |> List.walk 0 \acc, { numbers, result } ->
        when numbers is
            [first, .. as rest] ->
                if isSolvable { numbers: rest, result } first part then
                    acc + result
                else
                    acc

            _ -> acc

    |> Num.toStr
    |> Ok

Equation : { result : U64, numbers : List U64 }

puzzleParser : Parser (List U8) (List Equation)
puzzleParser =
    equationParser |> sepBy (string "\n")

equationParser : Parser (List U8) Equation
equationParser =
    const \result -> \numbers -> { result, numbers }
    |> keep digits
    |> skip (string ": ")
    |> keep (digits |> sepBy (string " "))

isSolvable = \{ result, numbers }, cur, part ->
    # dbg (numbers, operators, cur)
    when numbers is
        [] -> cur == result
        [a, .. as rest] ->
            if isSolvable { result, numbers: rest } (cur + a) part then
                Bool.true
            else if isSolvable { result, numbers: rest } (cur * a) part then
                Bool.true
            else if part == Part1 then
                Bool.false
            else
                next = combineOperator cur a
                isSolvable { result, numbers: rest } next part

combineOperator = \a, b ->
    len = numLen b 0
    ten = Num.powInt 10 len
    a * ten + b

numLen = \a, result ->
    # This should be implemented with Num.log10 if it would exist
    if a // 10 == 0 then
        result + 1
    else
        numLen (a // 10) (result + 1)

expect
    got = combineOperator 12 34
    expected = 1234
    got == expected

expect
    got = part2 example
    expected = Ok "11387"
    got == expected

part2 = \input ->
    solve input Part2
