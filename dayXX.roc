app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.0/je3X2cSdUa6b24fO1SS_vGNS5MwU-a-3r1niP_7iG6k.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

examplePart1 =
    "the example for part 1"

expect part1 examplePart1 == Ok "the example for part 1"

part1 = \input ->
    input
    |> Ok
    |> Result.mapErr \_ -> ThisLineIsNecessaryForRoc

examplePart2 =
    "example for part 2"

expect part2 examplePart2 == Ok "2 trap rof elpmaxe"

part2 = \input ->
    input
    |> Str.toUtf8
    |> List.reverse
    |> Str.fromUtf8
