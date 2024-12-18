app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser, sepBy, const, keep, skip]
import parser.String exposing [parseStr, string, digits]

part1 = \input ->
    input
    |> parse?
    |> compute?
    |> outputToStr
    |> Ok

Device : {
    program : List U64,
    registerA : U64,
    registerB : U64,
    registerC : U64,
    instructionPointer : U64,
    output : List U64,
}

compute : Device -> Result Device _
compute = \device ->
    when listGet2 device.program device.instructionPointer is
        Ok (instruction, operant) ->
            callOperant? instruction operant device
            |> compute

        Err OutOfBounds ->
            Ok device

callOperant : U64, U64, Device -> Result Device _
callOperant = \instruction, operant, device ->
    when instruction is
        0 ->
            numerator = device.registerA
            denominator = 2 |> Num.powInt (getComboOperant? operant device)

            device
            |> setRegisterA (numerator // denominator)
            |> incInstructoinPointer
            |> Ok

        1 ->
            v1 = device.registerB
            v2 = operant |> Num.toU64

            device
            |> setRegisterB (Num.bitwiseXor v1 v2)
            |> incInstructoinPointer
            |> Ok

        2 ->
            combo = getComboOperant? operant device
            device
            |> setRegisterB (combo % 8)
            |> incInstructoinPointer
            |> Ok

        3 ->
            if device.registerA == 0 then
                device
                |> incInstructoinPointer
                |> Ok
            else
                { device & instructionPointer: operant |> Num.toU64 }
                |> Ok

        4 ->
            v1 = device.registerB
            v2 = device.registerC

            device
            |> setRegisterB (Num.bitwiseXor v1 v2)
            |> incInstructoinPointer
            |> Ok

        5 ->
            combo = getComboOperant? operant device
            device
            |> print (combo % 8)
            |> incInstructoinPointer
            |> Ok

        6 ->
            numerator = device.registerA
            denominator = 2 |> Num.powInt (getComboOperant? operant device)

            device
            |> setRegisterB (numerator // denominator)
            |> incInstructoinPointer
            |> Ok

        7 ->
            numerator = device.registerA
            denominator = 2 |> Num.powInt (getComboOperant? operant device)

            device
            |> setRegisterC (numerator // denominator)
            |> incInstructoinPointer
            |> Ok

        _ ->
            Err (IllegalInstruction instruction)

print = \device, value ->
    { device & output: List.append device.output value }

incInstructoinPointer = \device ->
    { device & instructionPointer: device.instructionPointer + 2 }

setRegisterA = \device, value ->
    { device & registerA: value }

setRegisterB = \device, value ->
    { device & registerB: value }

setRegisterC = \device, value ->
    { device & registerC: value }

getComboOperant : U64, Device -> Result U64 _
getComboOperant = \operant, device ->
    when operant is
        0 | 1 | 2 | 3 -> operant |> Ok
        4 -> device.registerA |> Ok
        5 -> device.registerB |> Ok
        6 -> device.registerC |> Ok
        _ -> Err (IllegalOperant operant)

part2 = \input ->
    input
    |> parse?
    |> part2Helper? 0 0
    |> Num.toStr
    |> Ok

part2Helper : Device, U64, U64 -> Result U64 _
part2Helper = \device, index, result ->
    if index == List.len device.program then
        Ok result
        else

    expected = device.program |> List.takeLast (index + 1)

    newResults : List U64
    newResults =
        List.range { start: At (result * 8), end: Length 8 }
        |> List.walkTry? [] \acc, a ->
            output =
                device
                |> setRegisterA a
                |> compute?
                |> .output
            if output == expected then
                acc |> List.append a |> Ok
            else
                acc |> Ok

    when newResults is
        [] ->
            Err NoResult

        _ ->
            List.walkUntil newResults (Err NotFound) \state, r ->
                when part2Helper device (index + 1) r is
                    Err NoResult ->
                        Continue state

                    Err err ->
                        Break (Err err)

                    Ok finalResult ->
                        Break (Ok finalResult)

outputToStr = \device ->
    device.output
    |> List.map Num.toStr
    |> Str.joinWith ","

parse : Str -> Result Device _
parse = \input ->
    parseStr deviceParser input

deviceParser : Parser (List U8) Device
deviceParser =
    const \registerA -> \registerB -> \registerC -> \program -> {
                    registerA,
                    registerB,
                    registerC,
                    program,
                    instructionPointer: 0,
                    output: [],
                }
    |> skip (string "Register A: ")
    |> keep (digits)
    |> skip (string "\nRegister B: ")
    |> keep (digits)
    |> skip (string "\nRegister C: ")
    |> keep (digits)
    |> skip (string "\n\nProgram: ")
    |> keep (digits |> sepBy (string ","))

listGet2 = \list, index ->
    v1 = List.get? list index
    v2 = List.get? list (index + 1)
    Ok (v1, v2)

expect
    example =
        """
        Register A: 729
        Register B: 0
        Register C: 0

        Program: 0,1,5,4,3,0
        """
    got = part1 example
    expected = Ok "4,6,3,5,6,3,5,2,1,0"
    got == expected

expect
    example =
        """
        Register A: 2024
        Register B: 0
        Register C: 0

        Program: 0,3,5,4,3,0
        """
    got = part2 example
    expected = Ok "117440"
    got == expected
