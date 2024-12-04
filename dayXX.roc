app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.7/Tg23npX1TEGNlsYqX1JfrdtvW4OlwLdvsFnJMUJNZSU.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

example =
    """
    the example for
    part 1
    """

expect
    got = part1 example
    expected = Ok "TODO"
    got == expected

part1 = \_input ->
    Err TODO

expect
    got = part2 example
    expected = Ok "TODO"
    got == expected

part2 = \_input ->
    Err TODO
