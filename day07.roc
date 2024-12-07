app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
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
    equations = parseStr? puzzleParser (input |> Str.trim)

    equations
    |> List.walk 0 \acc, { numbers, result } ->
        when numbers is
            [first, .. as rest] ->
                if isSolvable { numbers: rest, result } first then
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

isSolvable = \{ result, numbers }, cur ->
    # dbg (numbers, operators, cur)
    when numbers is
        [] -> cur == result
        [a, .. as rest] ->
            if isSolvable { result, numbers: rest } (cur + a) then
                Bool.true
            else
                isSolvable { result, numbers: rest } (cur * a)

expect
    got = part2 example
    expected = Ok "TODO"
    got == expected

part2 = \_input ->
    Err TODO
