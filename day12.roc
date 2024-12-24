app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

example =
    """
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """

expect
    got = part1 example
    expected = Ok "1930"
    got == expected

part1 = \input ->
    map = input |> parse?

    map
    |> findRegions
    |> List.map \{ indexes, element } ->
        perimeter = countPerimeter map element indexes
        List.len indexes * perimeter
    |> List.sum
    |> Num.toStr
    |> Ok

Map : { data : List U8, size : U64 }

parse : Str -> Result Map _
parse = \input ->
    list = input |> Str.toUtf8
    size = list |> List.findFirstIndex? \e -> e == '\n'
    data = list |> List.dropIf \e -> e == '\n'
    Ok { data, size }

findRegions : Map -> List { indexes : List U64, element : U8 }
findRegions = \map ->
    { start: At 0, end: Before (List.len map.data) }
    |> List.range
    |> List.walk { map, regions: [] } \{ map: accMap, regions }, index ->
        when List.get accMap.data index is
            Ok element if isUpper element ->
                (indexes, newMap) = walkRegion accMap element [index] []
                { map: newMap, regions: List.append regions { indexes, element } }

            _ -> { map: accMap, regions }
    |> \{ regions } -> regions

walkRegion : Map, U8, List U64, List U64 -> (List U64, Map)
walkRegion = \map, element, indexes, result ->
    when indexes is
        [] ->
            (result, map)

        [firstIndex, .. as rest] ->
            if List.get map.data firstIndex == Ok element then
                newMap = { map & data: List.update map.data firstIndex toLower }
                foundAround =
                    around map firstIndex
                    |> List.keepIf \(e, _) -> e == element
                    |> List.map \(_, i) -> i

                walkRegion newMap element (rest |> List.concat foundAround) (result |> List.append firstIndex)
            else
                walkRegion map element rest result

countPerimeter = \map, element, indexes ->
    indexes
    |> List.walk 0 \acc, index ->
        same =
            around map index
            |> List.countIf \(e, _) -> e == element

        acc + 4 - same

around : Map, U64 -> List (U8, U64)
around = \map, index ->
    [
        topElementIndex map index,
        rightElementIndex map index,
        bottomElementIndex map index,
        leftElementIndex map index,
    ]
    |> List.keepOks \e -> e

topElementIndex = \map, index ->
    nIndex = Num.subChecked? index map.size
    List.get map.data nIndex
    |> Result.map \e -> (e, nIndex)

bottomElementIndex = \map, index ->
    nIndex = index + map.size
    List.get map.data nIndex
    |> Result.map \e -> (e, nIndex)

rightElementIndex = \map, index ->
    nIndex = index + 1
    if nIndex % map.size == 0 then
        Err OutOfBounds
    else
        List.get map.data nIndex
        |> Result.map \e -> (e, nIndex)

leftElementIndex = \map, index ->
    if index % map.size == 0 then
        Err OutOfBounds
    else
        nIndex = Num.subChecked? index 1
        List.get map.data nIndex
        |> Result.map \e -> (e, nIndex)

isUpper = \e ->
    e >= 'A' && e <= 'Z'

toLower = \e ->
    e + 'A' - 'a'

part2 = \input ->
    map = input |> parse?

    map
    |> findRegions
    |> List.map \region ->
        (List.len region.indexes) * (countFence map region)
    |> List.sum
    |> Inspect.toStr
    |> Ok

countFence = \map, { element, indexes } ->
    indexes
    |> List.sortAsc
    |> List.walk { top: [], left: [], bottom: [], right: [] } \acc, index ->
        topElement = topElementIndex map index |> Result.map .0
        rightElement = rightElementIndex map index |> Result.map .0
        leftElement = leftElementIndex map index |> Result.map .0
        bottomElement = bottomElementIndex map index |> Result.map .0

        row = (index // map.size)
        col = (index % map.size)

        acc2 =
            if topElement != Ok element then
                { acc & top: List.append acc.top (row, col) }
            else
                acc

        acc3 =
            if rightElement != Ok element then
                { acc2 & right: List.append acc2.right (col, row) }
            else
                acc2

        acc4 =
            if bottomElement != Ok element then
                { acc3 & bottom: List.append acc3.bottom (row, col) }
            else
                acc3

        acc5 =
            if leftElement != Ok element then
                { acc4 & left: List.append acc4.left (col, row) }
            else
                acc4

        acc5
    |> \{ top, bottom, left, right } ->
        (top |> countSides 1)
        +
        (bottom |> countSides 1)
        +
        (left |> List.sortWith (\(a, _), (b, _) -> Num.compare a b) |> countSides 1)
        +
        (right |> List.sortWith (\(a, _), (b, _) -> Num.compare a b) |> countSides 1)

countSides = \sides, result ->
    when sides is
        [] | [_] -> result
        [(a1, b1), (a2, b2), ..] ->
            if a1 == a2 && b1 + 1 == b2 then
                countSides (List.dropFirst sides 1) result
            else
                countSides (List.dropFirst sides 1) (result + 1)

expect
    got = part2
        """
        AAAA
        BBCD
        BBCC
        EEEC
        """
    expected = Ok "80"
    got == expected

expect
    got = part2
        """
        OOOOO
        OXOXO
        OOOOO
        OXOXO
        OOOOO
        """
    expected = Ok "436"
    got == expected

expect
    got = part2
        """
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
        """
    expected = Ok "236"
    got == expected

expect
    got = part2
        """
        AAAAAA
        AAABBA
        AAABBA
        ABBAAA
        ABBAAA
        AAAAAA
        """
    expected = Ok "368"
    got == expected

expect
    got = part2 example
    expected = Ok "1206"
    got == expected
