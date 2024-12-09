app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

example =
    "2333133121414131402"

expect
    got = part1 example
    expected = Ok "1928"
    got == expected

part1 = \input ->
    numbers = parse input
    fromFront = numbers |> everySecond
    fromBehind = numbers |> List.reverse |> everySecond
    maxFileID = fromBehind |> List.len |> Num.sub 1
    idProvider = idProviderInit 0 maxFileID fromFront

    numbers
    |> List.walkWithIndex (0, 0, idProvider) \(result, resultIndex, ip), amount, index ->
        if index % 2 == 0 then
            calcChecksum resultIndex ip amount result Front
            |> Result.withDefault (result, resultIndex, ip)
        else
            calcChecksum resultIndex ip amount result Back
            |> Result.withDefault (result, resultIndex, ip)

    |> \(a, _, _) -> a
    |> Num.toStr
    |> Ok

calcChecksum = \resultIndex, idProvider, times, result, side ->
    if times == 0 then
        Ok (result, resultIndex, idProvider)
    else
        (id, newProvider) = getNextID? idProvider side
        calcChecksum (resultIndex + 1) newProvider (times - 1) (result + id * resultIndex) side

parse = \input ->
    input
    |> Str.trim
    |> Str.toUtf8
    |> List.map \e -> e - '0'

everySecond = \list ->
    list
    |> List.walkWithIndex (List.withCapacity (List.len list // 2 + 1)) \acc, elem, index ->
        if index % 2 == 0 then
            List.append acc elem
        else
            acc

IDProvider := { firstID : U64, lastID : U64, list : List U8 }

idProviderInit = \firstID, lastID, list ->
    @IDProvider { firstID, lastID, list }

getNextID : IDProvider, [Front, Back] -> Result (U64, IDProvider) [Done]
getNextID = \idProvider, side ->
    when side is
        Front -> getNextIDFront idProvider
        Back -> getNextIDBack idProvider

getNextIDFront : IDProvider -> Result (U64, IDProvider) [Done]
getNextIDFront = \@IDProvider { firstID, lastID, list } ->
    when list is
        [] ->
            Err Done

        [first, .. as rest] ->
            if first == 0 then
                getNextIDFront (@IDProvider { firstID: firstID + 1, lastID, list: rest })
            else
                Ok (firstID, @IDProvider { firstID, lastID, list: list |> List.update 0 \old -> old - 1 })

getNextIDBack : IDProvider -> Result (U64, IDProvider) [Done]
getNextIDBack = \@IDProvider { firstID, lastID, list } ->
    when list is
        [] ->
            Err Done

        [.. as rest, last] ->
            if last == 0 then
                getNextIDBack (@IDProvider { firstID, lastID: lastID - 1, list: rest })
            else
                lastElement = list |> List.len |> Num.sub 1
                Ok (lastID, @IDProvider { firstID, lastID, list: list |> List.update lastElement \old -> old - 1 })

expect
    got = part2 example
    expected = Ok "2858"
    got == expected

part2 = \_input ->
    Err TODO
