app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

Position : { row : U64, col : U64 }
Direction : [Top, Right, Bottom, Left]
Map : Dict Position [Wall, Box]

part1 = \input ->
    input
    |> parse?
    |> walk
    |> mapToNumber
    |> Num.toStr
    |> Ok

walk = \{ map, start, moves } ->
    moves
    |> List.walk { map, cur: start } walkStep
    |> \{ map: endMap } -> endMap

walkStep : { map : Map, cur : Position }, Direction -> { map : Map, cur : Position }
walkStep = \{ map, cur }, direction ->
    nextPos = nextPosition cur direction
    when Dict.get map nextPos is
        Err KeyNotFound -> { map, cur: nextPos }
        Ok Wall -> { map, cur }
        Ok Box ->
            when walkBoxes map nextPos direction is
                Err Blocked -> { map, cur }
                Ok freePos ->
                    map
                    |> Dict.remove nextPos
                    |> Dict.insert freePos Box
                    |> \newMap -> { map: newMap, cur: nextPos }

nextPosition = \{ col, row }, direction ->
    when direction is
        Top -> { row: row - 1, col }
        Right -> { col: col + 1, row }
        Bottom -> { row: row + 1, col }
        Left -> { col: col - 1, row }

walkBoxes : Map, Position, Direction -> Result Position [Blocked]
walkBoxes = \map, cur, direction ->
    when Dict.get map cur is
        Err KeyNotFound -> Ok cur
        Ok Wall -> Err Blocked
        Ok Box ->
            walkBoxes map (nextPosition cur direction) direction

mapToNumber = \map ->
    map
    |> Dict.walk 0 \acc, { row, col }, elem ->
        when elem is
            Box -> acc + row * 100 + col
            Wall -> acc

part2 = \input ->
    input
    |> parse?
    |> modifyForPart2
    |> walkPart2
    |> mapToNumber
    |> Num.toStr
    |> Ok

modifyForPart2 = \{ map, start: { row: startRow, col: startCol }, moves } ->
    newMap =
        map
        |> Dict.walk (Dict.withCapacity (Dict.len map)) \newDict, { row, col: oldCol }, value ->
            newDict
            |> Dict.insert { row, col: oldCol * 2 } value
            |> \d ->
                when value is
                    Wall -> d |> Dict.insert { row, col: oldCol * 2 + 1 } value
                    Box -> d
    newStart = { row: startRow, col: startCol * 2 }
    { map: newMap, start: newStart, moves }

walkPart2 = \{ map, start, moves } ->
    moves
    |> List.walk { map, cur: start } walkStepPart2
    |> \{ map: endMap } -> endMap

walkStepPart2 : { map : Map, cur : Position }, Direction -> { map : Map, cur : Position }
walkStepPart2 = \{ map, cur }, direction ->
    nextPos = nextPosition cur direction
    when Dict.get map nextPos is
        Ok Wall -> { map, cur }
        Ok Box ->
            when walkAndUpdateBoxes map nextPos direction is
                Ok newMap ->
                    { map: newMap, cur: nextPos }

                Err Blocked ->
                    { map, cur }

        Err KeyNotFound ->
            nextPosLeft = nextPosition nextPos Left
            if direction != Right && Dict.get map nextPosLeft == Ok Box then
                when walkAndUpdateBoxes map nextPosLeft direction is
                    Ok newMap ->
                        { map: newMap, cur: nextPos }

                    Err Blocked ->
                        { map, cur }
            else
                { map, cur: nextPos }

walkAndUpdateBoxes = \map, cur, direction ->
    mayBoxIndexList =
        when direction is
            Left | Right ->
                findBoxesLeftRight map cur direction []

            Top | Bottom ->
                findBoxesTopBottom map [cur] direction [cur]

    mayBoxIndexList
    |> Result.map \boxIndexList ->
        nextMap =
            boxIndexList
            |> List.walk map \acc, index ->
                acc |> Dict.remove index

        boxIndexList
        |> List.walk nextMap \acc, index ->
            acc |> Dict.insert (index |> nextPosition direction) Box

findBoxesLeftRight : Map, Position, Direction, List Position -> Result (List Position) _
findBoxesLeftRight = \map, cur, direction, result ->
    when Dict.get map cur is
        Ok Box ->
            nextPos = cur |> nextPosition direction
            if direction == Left then
                nextPosLeft = cur |> nextPosition direction
                nextPosVal = Dict.get map nextPosLeft
                if nextPosVal == Ok Wall then
                    Err Blocked
                else
                    findBoxesLeftRight map (nextPos |> nextPosition direction) direction (result |> List.append cur)
            else
                findBoxesLeftRight map (nextPos |> nextPosition direction) direction (result |> List.append cur)

        Err KeyNotFound ->
            Ok result

        Ok Wall ->
            if direction == Left then
                Ok result
            else
                Err Blocked

findBoxesTopBottom = \map, indexList, direction, result ->
    when indexList is
        [] -> Ok result
        [first, .. as rest] ->
            nextPos = nextPosition first direction
            nextPosLeft = nextPosition nextPos Left
            nextPosRight = nextPosition nextPos Right
            val = Dict.get map nextPos
            valLeft = Dict.get map nextPosLeft
            valRight = Dict.get map nextPosRight

            if val == Ok Wall || valRight == Ok Wall then
                Err Blocked
                else

            newBoxes =
                [(nextPosLeft, valLeft), (nextPos, val), (nextPosRight, valRight)]
                |> List.keepIf \(_, v) -> v == Ok Box
                |> List.map \(p, _) -> p

            findBoxesTopBottom map (rest |> List.concat newBoxes) direction (result |> List.concat newBoxes)

parse : Str -> Result { map : Map, start : Position, moves : List Direction } _
parse = \input ->
    when input |> Str.splitOn "\n\n" is
        [inputMap, inputMoves] ->
            (map, start) = parseMap? inputMap
            moves = parseMoves? inputMoves
            Ok { map, start, moves }

        _ ->
            Err InvalidInput

parseMap = \input ->
    input
    |> Str.walkUtf8 { start: Err NoStart, row: 0, col: 0, map: Dict.empty {} } \acc, elem ->
        row = acc.row
        col = acc.col
        when elem is
            '#' -> { acc & map: Dict.insert acc.map { row, col } Wall, col: col + 1 }
            'O' -> { acc & map: Dict.insert acc.map { row, col } Box, col: col + 1 }
            '@' -> { acc & start: Ok { row, col }, col: col + 1 }
            '\n' -> { acc & row: row + 1, col: 0 }
            _ -> { acc & col: col + 1 }
    |> \{ start, map } ->
        start
        |> Result.map \pos -> (map, pos)

parseMoves : Str -> Result (List Direction) _
parseMoves = \input ->
    input
    |> Str.toUtf8
    |> List.walkTry (List.withCapacity (Str.countUtf8Bytes input)) \acc, elem ->
        when elem is
            '^' -> acc |> List.append Top |> Ok
            '>' -> acc |> List.append Right |> Ok
            'v' -> acc |> List.append Bottom |> Ok
            '<' -> acc |> List.append Left |> Ok
            '\n' -> acc |> Ok
            _ -> Err InvalidMove

exampleLarge =
    """
    ##########
    #..O..O.O#
    #......O.#
    #.OO..O.O#
    #..O@..O.#
    #O#..O...#
    #O..O..O.#
    #.OO.O.OO#
    #....O...#
    ##########

    <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
    vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
    ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
    <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
    ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
    ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
    >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
    <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
    ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
    v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    """

exampleSmall =
    """
    ########
    #..O.O.#
    ##@.O..#
    #...O..#
    #.#.O..#
    #...O..#
    #......#
    ########

    <^^>>>vv<v>>v<<
    """

expect
    got = part1 exampleLarge
    expected = Ok "10092"
    got == expected

expect
    got = part1 exampleSmall
    expected = Ok "2028"
    got == expected

expect
    example =
        """
        #######
        #...#.#
        #.....#
        #..OO@#
        #..O..#
        #.....#
        #######

        <vv<<^^<<^^
        """

    got = part2 example
    expected = Ok "618"
    got == expected

expect
    got = part2 exampleLarge
    expected = Ok "9021"
    got == expected
