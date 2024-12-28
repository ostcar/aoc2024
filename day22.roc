app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry? \line ->
        line
        |> Str.toU64?
        |> nTimes iterate 2000
        |> Ok
    |> List.sum
    |> Num.toStr
    |> Ok

part2 = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry? \line ->
        line
        |> Str.toU64?
        |> findDiffs iterate 2000 (Err NoValue) (Err NoValue, Err NoValue, Err NoValue, Err NoValue) (Dict.withCapacity 2000)
        |> Ok
    |> List.walk (Dict.withCapacity 2000) \combined, dict ->
        dictCombine combined dict Num.add
    |> Dict.values
    |> List.max
    |> Result.withDefault 0
    |> Num.toStr
    |> Ok

nTimes = \value, fn, n ->
    if n == 0 then
        value
    else
        nTimes (fn value) fn (n - 1)

iterate = \number ->
    v1 =
        number
        |> Num.shiftLeftBy 6 # Same as * 2^6
        |> Num.bitwiseXor number
        |> Num.bitwiseAnd 16777215 # Same as % 2^24

    v2 =
        v1
        |> Num.shiftRightZfBy 5 # Same as // 2^5
        |> Num.bitwiseXor v1
        |> Num.bitwiseAnd 16777215

    v3 =
        v2
        |> Num.shiftLeftBy 11 # Same as 2^11
        |> Num.bitwiseXor v2
        |> Num.bitwiseAnd 16777215

    v3

findDiffs : U64, _, U64, Result I64 _, _, Dict (I8, I8, I8, I8) I64 -> Dict (I8, I8, I8, I8) I64
findDiffs = \value, fn, n, lastlastDigit, lastDiffs, result ->
    lastDigit = value % 10 |> Num.toI64
    diff = lastlastDigit |> Result.map \v -> (lastDigit - v) |> Num.toI8
    newLastDiffs =
        v1 = diff
        v2 = lastDiffs.0
        v3 = lastDiffs.1
        v4 = lastDiffs.2
        (v1, v2, v3, v4)

    newResult =
        when newLastDiffs is
            (Ok a, Ok b, Ok c, Ok d) ->
                dictInsertIfNotExist result (a, b, c, d) lastDigit

            _ -> result

    if n == 0 then
        result
    else
        findDiffs (fn value) fn (n - 1) (Ok lastDigit) newLastDiffs newResult

dictInsertIfNotExist = \dict, index, value ->
    Dict.update dict index \r -> r |> Result.onErr \_ -> Ok value

dictCombine : Dict k v, Dict k v, (v, v -> v) -> Dict k v
dictCombine = \dict1, dict2, fn ->
    dict2
    |> Dict.walk dict1 \acc, key, v1 ->
        acc
        |> Dict.update key \r ->
            when r is
                Err Missing ->
                    Ok v1

                Ok v2 ->
                    Ok (fn v1 v2)

expect
    example =
        """
        1
        10
        100
        2024
        """
    got = part1 example
    expected = Ok "37327623"
    got == expected

expect
    example =
        """
        1
        2
        3
        2024
        """
    got = part2 example
    expected = Ok "23"
    got == expected
