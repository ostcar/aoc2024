app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
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
        top map index,
        right map index,
        bottom map index,
        left map index,
    ]
    |> List.keepOks \e -> e

top = \map, index ->
    nIndex = Num.subChecked? index map.size
    List.get map.data nIndex
    |> Result.map \e -> (e, nIndex)

bottom = \map, index ->
    nIndex = index + map.size
    List.get map.data nIndex
    |> Result.map \e -> (e, nIndex)

right = \map, index ->
    nIndex = index + 1
    if nIndex % map.size == 0 then
        Err OutOfBounds
    else
        List.get map.data nIndex
        |> Result.map \e -> (e, nIndex)

left = \map, index ->
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

# expect
#    got = part2
#        """
#        AAAA
#        BBCD
#        BBCC
#        EEEC
#        """
#    expected = Ok "80"
#    got == expected

# expect
#    got = part2
#        """
#        OOOOO
#        OXOXO
#        OOOOO
#        OXOXO
#        OOOOO
#        """
#    expected = Ok "436"
#    got == expected

expect
    got = part2
        """
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
        """
    expected = Ok "204"
    got == expected

# expect
#    got = part2
#        """
#        AAAAAA
#        AAABBA
#        AAABBA
#        ABBAAA
#        ABBAAA
#        AAAAAA
#        """
#    expected = Ok "368"
#    got == expected

# expect
#    got = part2 example
#    expected = Ok "1206"
#    got == expected

# 852920 to low
part2 = \input ->
    map = input |> parse?

    map
    |> findRegions
    |> List.map \{ indexes, element } ->
        perimeter = countSides map indexes
        dbg (perimeter, element)
        List.len indexes * perimeter
    |> List.sum
    |> Num.toStr
    |> Ok

countSides : Map, List U64 -> U64
countSides = \map, indexes ->
    when indexes is
        [] -> 0
        [_] | [_, _] -> 4
        [start, ..] ->
            inRegion = Set.fromList indexes
            walkBorder map start inRegion start Right 0

isDone : Map, U64, Set U64, U64, _ -> Bool
isDone = \map, start, inRegion, current, direction ->
    if direction == Right then
        Bool.false
    else if start != current then
        Bool.false
    else if direction != Left then
        Bool.true
    else
        when bottom map start is
            Ok (_, bottomIndex) ->
                Set.contains inRegion bottomIndex |> Bool.not

            _ ->
                Bool.true

walkBorder = \map, start, inRegion, current, direction, result ->
    dbg (current, direction, result)
    if isDone map start inRegion current direction then
        add = if direction == Left then 2 else 1
        result + add
        else

    when nextPlace map current (turnLeft direction) is
        Ok (_, nextIndex) if Set.contains inRegion nextIndex ->
            walkBorder map start inRegion nextIndex (turnLeft direction) (result + 1)

        _ ->
            when nextPlace map current direction is
                Ok (_, nextIndex) if Set.contains inRegion nextIndex ->
                    walkBorder map start inRegion nextIndex direction result

                _ ->
                    when nextPlace map current (turnRight direction) is
                        Ok (_, nextIndex) if Set.contains inRegion nextIndex ->
                            walkBorder map start inRegion nextIndex (turnRight direction) (result + 1)

                        _ ->
                            walkBorder map start inRegion current (turnLeft direction |> turnLeft) (result + 2)

nextPlace = \map, index, direction ->
    when direction is
        Right -> right map index
        Left -> left map index
        Bottom -> bottom map index
        Top -> top map index

turnLeft = \direction ->
    when direction is
        Right -> Top
        Top -> Left
        Left -> Bottom
        Bottom -> Right

turnRight = \direction ->
    when direction is
        Right -> Bottom
        Bottom -> Left
        Left -> Top
        Top -> Right
