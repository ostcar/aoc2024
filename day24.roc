app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

import ascii.Ascii

part1 = \input ->
    input
    |> parse?
    |> solveAllZ 0 0
    |> Num.toStr
    |> Ok

part2 = \input ->
    store = input |> parse?
    reversed = buildReversedStore store
    firstCarry = reversed |> Dict.get? (Gate "x00" AND "y00")

    runFullAdderChecks? store reversed firstCarry 1 []
    |> List.mapTry? Ascii.fromStr
    |> Ascii.sortAsc
    |> List.map Ascii.toStr
    |> Str.joinWith ","
    |> Ok

Store : Dict Str [Init U64, Gate Str [AND, OR, XOR] Str]

runFullAdderChecks : Dict _ _, Dict _ _, Str, U64, List Str -> Result _ _
runFullAdderChecks = \store, reversed, carry, n, result ->
    if n == 44 then
        # TODO: check last carry with z45
        Ok result
        else

    when isFullAdder reversed n carry is
        Ok newCarry ->
            runFullAdderChecks store reversed newCarry (n + 1) result

        Err (Switch s1 s2) ->
            newStore =
                gate1 = Dict.get? store s1
                gate2 = Dict.get? store s2
                store
                |> Dict.insert s1 gate2
                |> Dict.insert s2 gate1
            newReversed = buildReversedStore newStore
            runFullAdderChecks newStore newReversed carry n (result |> List.append s1 |> List.append s2)

        Err err ->
            Err (ErrorInBit err n carry)

buildReversedStore = \store ->
    store
    |> Dict.walk (Dict.empty {}) \dict, key, value ->
        when value is
            Gate a gate b ->
                dict
                |> Dict.insert (Gate a gate b) key
                |> Dict.insert (Gate b gate a) key

            _ -> dict

isFullAdder : Dict _ Str, U64, Str -> Result Str [Switch Str Str]_
isFullAdder = \reversedGates, z, carry ->
    zN = numberedKey "z" z
    xN = numberedKey "x" z
    yN = numberedKey "y" z

    xXORy = reversedGates |> Dict.get? (Gate xN XOR yN)
    xANDy = reversedGates |> Dict.get? (Gate xN AND yN)
    gateS = reversedGates |> Dict.get (Gate xXORy XOR carry) |> Result.mapErr? \_ -> Switch xXORy xANDy

    if gateS != zN then
        Err (Switch gateS zN)
        else

    preC = reversedGates |> Dict.get? (Gate xXORy AND carry)
    newCarry = reversedGates |> Dict.get? (Gate preC OR xANDy)

    Ok newCarry

numberedKey = \l, n ->
    if n < 10 then
        "$(l)0$(Num.toStr n)"
    else
        "$(l)$(Num.toStr n)"

solveAllZ : Store, U8, U64 -> U64
solveAllZ = \gates, z, result ->
    when solveZ gates z is
        Ok v ->
            v
            |> Num.shiftLeftBy z
            |> Num.bitwiseOr result
            |> \newResult ->
                solveAllZ gates (z + 1) newResult

        Err _ ->
            result

solveZ : Store, U8 -> Result U64 _
solveZ = \gates, z ->
    var = numberedKey "z" z
    solveValue gates var

solveValue : Store, Str -> Result U64 _
solveValue = \gates, value ->
    when Dict.get? gates value is
        Init v -> v |> Ok
        Gate arg1 AND arg2 ->
            (solveValue? gates arg1) |> Num.bitwiseAnd (solveValue? gates arg2) |> Ok

        Gate arg1 OR arg2 ->
            (solveValue? gates arg1) |> Num.bitwiseOr (solveValue? gates arg2) |> Ok

        Gate arg1 XOR arg2 ->
            (solveValue? gates arg1) |> Num.bitwiseXor (solveValue? gates arg2) |> Ok

parse : Str -> Result Store _
parse = \input ->
    when input |> Str.splitOn "\n\n" is
        [init, gates] ->
            initValues =
                init
                |> Str.splitOn "\n"
                |> List.mapTry? \line ->
                    when line |> Str.splitOn ": " is
                        [var, val] ->
                            Ok (var, Init (Str.toU64? val))

                        _ ->
                            Err InvalidInput
                |> Dict.fromList
                |> Ok

            gateValues =
                gates
                |> Str.splitOn "\n"
                |> List.mapTry? \line ->
                    when line |> Str.splitOn " " is
                        [arg1, gateStr, arg2, _, result] ->
                            gate =
                                when gateStr is
                                    "XOR" -> XOR
                                    "OR" -> OR
                                    "AND" -> AND
                                    _ ->
                                        return Err InvalidInput

                            Ok (result, Gate arg1 gate arg2)

                        _ -> Err InvalidInput
                |> Dict.fromList
                |> Ok

            Result.map2 initValues gateValues \a, b ->
                Dict.insertAll a b

        _ ->
            Err InvalidInput

expect
    example =
        """
        x00: 1
        x01: 1
        x02: 1
        y00: 0
        y01: 1
        y02: 0

        x00 AND y00 -> z00
        x01 XOR y01 -> z01
        x02 OR y02 -> z02
        """
    got = part1 example
    expected = Ok "4"
    got == expected

expect
    example =
        """
        x00: 1
        x01: 0
        x02: 1
        x03: 1
        x04: 0
        y00: 1
        y01: 1
        y02: 1
        y03: 1
        y04: 1

        ntg XOR fgs -> mjb
        y02 OR x01 -> tnw
        kwq OR kpj -> z05
        x00 OR x03 -> fst
        tgd XOR rvg -> z01
        vdt OR tnw -> bfw
        bfw AND frj -> z10
        ffh OR nrd -> bqk
        y00 AND y03 -> djm
        y03 OR y00 -> psh
        bqk OR frj -> z08
        tnw OR fst -> frj
        gnj AND tgd -> z11
        bfw XOR mjb -> z00
        x03 OR x00 -> vdt
        gnj AND wpb -> z02
        x04 AND y00 -> kjc
        djm OR pbm -> qhw
        nrd AND vdt -> hwm
        kjc AND fst -> rvg
        y04 OR y02 -> fgs
        y01 AND x02 -> pbm
        ntg OR kjc -> kwq
        psh XOR fgs -> tgd
        qhw XOR tgd -> z09
        pbm OR djm -> kpj
        x03 XOR y03 -> ffh
        x00 XOR y04 -> ntg
        bfw OR bqk -> z06
        nrd XOR fgs -> wpb
        frj XOR qhw -> z04
        bqk OR frj -> z07
        y03 OR x01 -> nrd
        hwm AND bqk -> z03
        tgd XOR rvg -> z12
        tnw OR pbm -> gnj
        """
    got = part1 example
    expected = Ok "2024"
    got == expected

expect
    example =
        """
        x00: 0
        x01: 1
        x02: 0
        x03: 1
        x04: 0
        x05: 1
        y00: 0
        y01: 0
        y02: 1
        y03: 1
        y04: 0
        y05: 1

        x00 AND y00 -> z05
        x01 AND y01 -> z02
        x02 AND y02 -> z01
        x03 AND y03 -> z03
        x04 AND y04 -> z04
        x05 AND y05 -> z00
        """
    got = part2 example
    expected = Ok "z00,z01,z02,z05"
    got == expected
