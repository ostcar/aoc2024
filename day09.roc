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
    maxFileID = fromFront |> List.len |> Num.sub 1
    idProvider = idProviderInit 0 maxFileID fromFront

    numbers
    |> List.walkWithIndex (0, 0, idProvider) \(result, resultIndex, ip), amount, index ->
        side =
            if index % 2 == 0 then
                Front
            else
                Back

        calcChecksum resultIndex ip amount result side
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

part2 = \input ->
    numbers = parse input
    data = toData numbers
    maxFileID = (numbers |> List.len) // 2

    { start: At maxFileID, end: At 0 }
    |> List.range
    |> List.walk data \acc, index ->
        defragmentOne acc index
    |> List.walkWithIndex 0 \acc, element, index ->
        when element is
            File nr ->
                acc + nr * index

            Empty ->
                acc
    |> Num.toStr
    |> Ok

toData : List U8 -> List [Empty, File U64]
toData = \numbers ->
    emptyData =
        numbers
        |> List.walk 0 \acc, e -> acc + (Num.toU64 e)
        |> List.withCapacity

    numbers
    |> List.walkWithIndex emptyData \acc, number, index ->
        space =
            if index % 2 == 0 then
                File (index // 2)
            else
                Empty
        acc
        |> List.concat (List.repeat space (number |> Num.toU64))

defragmentOne : List [Empty, File U64], U64 -> List [Empty, File U64]
defragmentOne = \list, fileID ->
    when findFile list fileID is
        Ok (start, end) ->
            len = end - start
            when findFreeSpace list len start 0 is
                Ok index ->
                    list
                    |> replaceChunk index (List.sublist list { start, len })
                    |> replaceChunk start (List.repeat Empty len)

                Err NotFound ->
                    list

        Err NotFound ->
            list

findFile = \list, file ->
    step =
        list
        |> List.walkWithIndexUntil NotFound \acc, element, index ->
            when acc is
                NotFound ->
                    if element == (File file) then
                        Continue (FoundStart index)
                    else
                        Continue NotFound

                FoundStart start ->
                    if element == (File file) then
                        Continue (FoundStart start)
                    else
                        Break (Found start index)

                Found start end ->
                    Break (Found start end)

    when step is
        NotFound -> Err NotFound
        FoundStart start ->
            Ok (start, list |> List.len)

        Found start end ->
            Ok (start, end)

findFreeSpace = \list, space, before, offset ->
    index =
        list
        |> List.findFirstIndex? \e -> e == Empty

    if (offset + index) >= before then
        Err NotFound
    else if list |> List.sublist { start: index, len: space } == List.repeat Empty space then
        Ok (offset + index)
    else
        list
        |> List.dropFirst (index + 1)
        |> findFreeSpace space before (offset + 1 + index)

replaceChunk = \list, index, replacement ->
    when replacement is
        [first, .. as rest] ->
            List.set list index first
            |> replaceChunk (index + 1) rest

        [] ->
            list
