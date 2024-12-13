app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser, sepBy, const, keep, skip, map]
import parser.String exposing [parseStr, string, digits]

example =
    """
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    """

expect
    got = part1 example
    expected = Ok "480"
    got == expected

part1 = \input ->
    parseStr? parse input
    |> List.map calcAB
    |> List.keepOks \v -> v
    |> List.map \(a, b) -> 3 * a + b
    |> List.sum
    |> Num.toStr
    |> Ok

Machine : { ax : F64, ay : F64, bx : F64, by : F64, px : F64, py : F64 }

parse : Parser (List U8) (List Machine)
parse =
    machineParser |> sepBy (string "\n\n")

machineParser : Parser (List U8) Machine
machineParser =
    const \ax -> \ay -> \bx -> \by -> \px -> \py -> { ax, ay, bx, by, px, py }
    |> skip (string "Button A: X+")
    |> keep (digits |> map Num.toF64)
    |> skip (string ", Y+")
    |> keep (digits |> map Num.toF64)
    |> skip (string "\nButton B: X+")
    |> keep (digits |> map Num.toF64)
    |> skip (string ", Y+")
    |> keep (digits |> map Num.toF64)
    |> skip (string "\nPrize: X=")
    |> keep (digits |> map Num.toF64)
    |> skip (string ", Y=")
    |> keep (digits |> map Num.toF64)

calcAB : Machine -> Result (U64, U64) _
calcAB = \{ ax, ay, bx, by, px, py } ->
    # Calculate b and a as float
    b = ((py - (ay * px / ax)) / (by - (ay * bx / ax)))
    a = (px / ax - (bx * b / ax))

    if isApproxInt a && isApproxInt b then
        Ok (a |> Num.round, b |> Num.round)
    else
        Err NoSolutionWithInts

isApproxInt = \a ->
    a
    |> Num.round
    |> Num.toF64
    |> isApproxEq a

expect
    got = part2 example
    expected = Ok "875318608908"
    got == expected

part2 = \input ->
    parseStr? parse input
    |> List.map changeForPart2
    |> List.map calcAB
    |> List.keepOks \v -> v
    |> List.map \(a, b) -> 3 * a + b
    |> List.sum
    |> Num.toStr
    |> Ok

changeForPart2 = \machine ->
    { machine & px: machine.px + 10000000000000, py: machine.py + 10000000000000 }

isApproxEq = \x, y ->
    eq = x <= y && x >= y
    meetsTolerance = Num.absDiff x y <= 0.01
    eq || meetsTolerance
