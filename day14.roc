app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser, sepBy, const, keep, skip, map]
import parser.String exposing [parseStr, string, digits, oneOf]

example =
    """
    p=0,4 v=3,-3
    p=6,3 v=-1,-3
    p=10,3 v=-1,2
    p=2,0 v=2,-1
    p=0,0 v=1,3
    p=3,0 v=-2,-2
    p=7,6 v=-1,-3
    p=3,0 v=-1,-2
    p=9,3 v=2,3
    p=7,3 v=-1,2
    p=2,4 v=2,-3
    p=9,5 v=-3,-3
    """

expect
    got = solvePart1 example 11 7
    expected = Ok "12"
    got == expected

part1 = \input ->
    solvePart1 input 101 103

solvePart1 = \input, width, height ->
    parseStr? parse input
    |> List.map \robot -> moveSteps robot 100 width height
    |> List.keepOks \robot -> inQuadrant robot width height
    |> List.walk [0, 0, 0, 0] \acc, elem ->
        List.update acc (elem |> Num.toU64) \q -> q + 1
    |> List.product
    |> Num.toStr
    |> Ok

Robot : { x : I64, y : I64, vx : I64, vy : I64 }

parse : Parser (List U8) (List Robot)
parse =
    machineParser |> sepBy (string "\n")

machineParser : Parser (List U8) Robot
machineParser =
    const \x -> \y -> \vx -> \vy -> { x, y, vx, vy }
    |> skip (string "p=")
    |> keep (digits |> map Num.toI64)
    |> skip (string ",")
    |> keep (digits |> map Num.toI64)
    |> skip (string " v=")
    |> keep signedDigits
    |> skip (string ",")
    |> keep signedDigits

signedDigits : Parser (List U8) I64
signedDigits =
    oneOf [
        (digits |> map Num.toI64),
        negativeNumber,
    ]

negativeNumber : Parser (List U8) I64
negativeNumber =
    const \v -> v |> Num.toI64 |> Num.mul -1
    |> skip (string "-")
    |> keep digits

moveSteps : Robot, I64, I64, I64 -> Robot
moveSteps = \{ x, y, vx, vy }, steps, width, height ->
    newX = (x + steps * width + steps * vx) % width
    newY = (y + steps * height + steps * vy) % height
    { x: newX, y: newY, vx, vy }

inQuadrant = \{ x, y }, width, height ->
    if x == width // 2 || y == height // 2 then
        Err InMiddle
        else

    vertical = x // ((width + 1) // 2)
    horizontal = y // ((height + 1) // 2)
    Ok (vertical + horizontal * 2)

expect
    got = solvePart2 example 11 7
    expected = Ok "12"
    got == expected

part2 = \input ->
    solvePart2 input 101 103

solvePart2 = \input, width, height ->
    parseStr? parse input
    |> findUniquePosition 0 width height
    |> Num.toStr
    |> Ok

findUniquePosition : List Robot, U64, I64, I64 -> U64
findUniquePosition = \robots, steps, width, height ->
    if allUnique robots then
        dbg (drawRobots robots width height)
        steps
        else

    robots
    |> List.map \robot -> moveSteps robot 1 width height
    |> findUniquePosition (steps + 1) width height

drawRobots = \robots, width, height ->
    line = List.repeat '.' (width |> Num.toU64)
    space = List.repeat line (height |> Num.toU64)

    robots
    |> List.walk space \acc, { x, y } ->
        acc
        |> List.update (y |> Num.toU64) \l ->
            List.set l (x |> Num.toU64) 'X'
    |> List.mapTry? Str.fromUtf8
    |> Str.joinWith "\n"
    |> Ok

allUnique = \robots ->
    len = robots |> List.len

    robots
    |> List.map \{ x, y } -> (x, y)
    |> Set.fromList
    |> Set.len
    |> \n -> n == len
