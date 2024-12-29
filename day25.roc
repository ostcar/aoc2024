app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
}

# I don't why, but I could not solve this. First, the platform failed, then I got strange errors and a wrong result.
# In the end, I looked at the subredit and converted this solution from python to roc:
# https://www.reddit.com/r/adventofcode/comments/1hlu4ht/comment/m3p50x5/
part1 = \input ->
    items = input |> parse

    items
    |> List.walk 0 \acc, s1 ->
        items
        |> List.walk acc \counter, s2 ->
            if Set.intersection s1 s2 |> Set.isEmpty then
                counter + 1
            else
                counter
    |> Num.divTrunc 2
    |> Num.toStr
    |> Ok

part2 = \_input ->
    Err TODO

parse = \input ->
    input
    |> Str.splitOn "\n\n"
    |> List.map \line ->
        line
        |> Str.toUtf8
        |> List.walkWithIndex (Set.empty {}) \acc, element, index ->
            if element == '#' then
                acc |> Set.insert index
            else
                acc

expect
    example =
        """
        #####
        .####
        .####
        .####
        .#.#.
        .#...
        .....

        #####
        ##.##
        .#.##
        ...##
        ...#.
        ...#.
        .....

        .....
        #....
        #....
        #...#
        #.#.#
        #.###
        #####

        .....
        .....
        #.#..
        ###..
        ###.#
        ###.#
        #####

        .....
        .....
        .....
        #....
        #.#..
        #.#.#
        #####
        """
    got = part1 example
    expected = Ok "3"
    got == expected
