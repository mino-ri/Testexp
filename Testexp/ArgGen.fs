module Testexp.ArgGen
open System
open Testexp.Implementations

type private RandomArgumentGenerator = RandomArgumentGenerator with
    interface IArgumentGenerator<uint64> with
        member _.Generate(state) =
            let (RandomState(seed)) = state
            let value = seed * 2147483649uL + 273051931uL
            state <- RandomState(value)
            value

/// Gets an argument generator that returns pseudo-random numbers. This generates the same numbers in the same order each time.
let random : IArgumentGenerator<uint64> = upcast RandomArgumentGenerator

[<Struct>]
type private BoundedArgumentGenerator private (maxExclusive: uint64, shiftBits: int) =
    new(maxExclusive: uint64) =
        let maxExclusive = max maxExclusive 1uL
        let shiftBits = 63 - System.Numerics.BitOperations.Log2(maxExclusive)
        BoundedArgumentGenerator(maxExclusive, shiftBits)

    member _.Generate(state: byref<RandomState>) =
        let mutable result = random.Generate(&state) >>> shiftBits
        while result >= maxExclusive do
            result <- random.Generate(&state) >>> shiftBits
        result

type private ForEachArgumentGenerator<'T>(items: 'T[]) =
    let mutable index = -1
    interface IArgumentGenerator<'T> with
        member _.Generate(_) =
            index <- min (items.Length - 1) (index + 1)
            items[index]

/// Creates an argument generator that returns the constant value.
let constant (value: 'T) =
    { new IArgumentGenerator<'T> with
        member _.Generate(_) = value    
    }

/// Represents the monad bind operator for ArgGen<T>. This is an inline function.
let inline bindInline ([<InlineIfLambda>] binding: 'T -> ArgGenCode<'U>) (source: IArgumentGenerator<'T>) : IArgumentGenerator<'U> =
    { new IArgumentGenerator<'U> with
        member _.Generate(state) = (binding (source.Generate(&state))).Invoke(&state)
    }

/// Represents the monad bind operator for ArgGen<T>.
let bind (binding: 'T -> IArgumentGenerator<'U>) source =
    source |> bindInline (fun value -> ArgGenCode<'U>(fun state -> (binding value).Generate(&state)))

/// Represents the functor map operator for ArgGen<T>. This is an inline function.
let inline mapInline ([<InlineIfLambda>] mapping: 'T -> 'U) (source: IArgumentGenerator<'T>) : IArgumentGenerator<'U> =
    { new IArgumentGenerator<'U> with
        member _.Generate(state) = mapping (source.Generate(&state))
    }

/// Represents the functor map operator for ArgGen<T>.
let map (mapping: 'T -> 'U) source = mapInline mapping source
    
/// Creates an argument generator from the generator function. This is an inline function.
let inline generatorInline ([<InlineIfLambda>] mapping: uint64 -> 'T) = mapInline mapping random

/// Creates an argument generator from the generator function.
let generator (mapping: uint64 -> 'T) = mapInline mapping random

let inline private chooseCore
    ([<InlineIfLambda>] mapping: 'T -> 'U)
    ([<InlineIfLambda>] predicate: 'U -> bool)
    ([<InlineIfLambda>] resultMapping: 'U -> 'V)
    (source: IArgumentGenerator<'T>) =
    { new IArgumentGenerator<'V> with
        member _.Generate(state) =
            let mutable result = mapping (source.Generate(&state))
            while not (predicate result) do
                result <- mapping (source.Generate(&state))
            resultMapping result
    }

/// Creates an argument generator that returns filtered value.
let filter (predicate: 'T -> bool) (source: IArgumentGenerator<'T>) =
    chooseCore id predicate id source

/// Creates an argument generator that returns filtered value.
let choose (chooser: 'T -> 'U option) (source: IArgumentGenerator<'T>) =
    chooseCore chooser Option.isSome Option.get source

/// Creates an argument generator that returns filtered value.
let choosev (chooser: 'T -> 'U voption) (source: IArgumentGenerator<'T>) =
    chooseCore chooser ValueOption.isSome ValueOption.get source

let inline private rangeCore (range: ^T) ([<InlineIfLambda>] mapping: uint64 -> ^U) =
    let boundedGen = BoundedArgumentGenerator(uint64 range)
    { new IArgumentGenerator< ^U > with
        member _.Generate(state) = mapping (boundedGen.Generate(&state))
    }

/// Creates an argument generator that returns random integers in the specified range.
let sbyteRange (minInclusive: sbyte) (maxExclusive: sbyte) =
    rangeCore (byte (maxExclusive - minInclusive)) (fun value -> sbyte value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let int16Range (minInclusive: int16) (maxExclusive: int16) =
    rangeCore (uint16 (maxExclusive - minInclusive)) (fun value -> int16 value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let int32Range (minInclusive: int32) (maxExclusive: int32) =
    rangeCore (uint32 (maxExclusive - minInclusive)) (fun value -> int32 value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let int64Range (minInclusive: int64) (maxExclusive: int64) =
    rangeCore (uint64 (maxExclusive - minInclusive)) (fun value -> int64 value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let byteRange minInclusive maxExclusive =
    rangeCore (maxExclusive - minInclusive) (fun value -> byte value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint16Range minInclusive maxExclusive =
    rangeCore (maxExclusive - minInclusive) (fun value -> uint16 value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint32Range minInclusive maxExclusive =
    rangeCore (maxExclusive - minInclusive) (fun value -> uint32 value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint64Range minInclusive maxExclusive =
    rangeCore (maxExclusive - minInclusive) (fun value -> value + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let intRange minInclusive maxExclusive = int32Range minInclusive maxExclusive

/// Creates an argument generator that returns random integers in the specified range.
let uintRange minInclusive maxExclusive = uint32Range minInclusive maxExclusive

/// Creates an argument generator that returns random chars in the specified range.
let charRange (minInclusive: char) (maxExclusive: char) =
    rangeCore (uint maxExclusive - uint minInclusive) (fun value -> char (uint value + uint minInclusive))

/// Gets an argument generator that returns any int8 value.
let int8 = generatorInline (fun value -> sbyte (value >>> 56))

/// Gets an argument generator that returns any int16 value.
let int16 = generatorInline (fun value -> int16 (value >>> 48))

/// Gets an argument generator that returns any int32 value.
let int32 = generatorInline (fun value -> int32 (value >>> 32))

/// Gets an argument generator that returns any int64 value.
let int64 = generatorInline int64

/// Gets an argument generator that returns any uint8 value.
let uint8 = generatorInline (fun value -> byte (value >>> 56))

/// Gets an argument generator that returns any uint16 value.
let uint16 = generatorInline (fun value -> uint16 (value >>> 48))

/// Gets an argument generator that returns any uint32 value.
let uint32 = generatorInline (fun value -> uint32 (value >>> 32))

/// Gets an argument generator that returns any uint64 value.
let uint64 = generatorInline uint64

/// Gets an argument generator that returns any sbyte value.
let sbyte = int8

/// Gets an argument generator that returns any byte value.
let byte = uint8

/// Gets an argument generator that returns any int value.
let int = int32

/// Gets an argument generator that returns any uint value.
let uint = uint32

/// Creates an argument generator that returns random ascii chars (0x20 - 0x7E).
let asciiChar = charRange ' ' '\x7F'

/// Creates an argument generator that returns random floating point numbers in the specified range.
let floatRange (minimum: float) (maximum: float) =
    let range = maximum - minimum
    generatorInline (fun value ->
        float (value % 2147483648uL) / 2147483648.0 * range + minimum
        |> min maximum |> max minimum)

/// Creates an argument generator that returns each items.
let forEach (items: 'T[]) : int * IArgumentGenerator<'T> =
    items.Length, upcast ForEachArgumentGenerator(items)

/// Creates an argument generator that returns lists containing the values generated by the specified generator.
let list (elementGenerator: IArgumentGenerator<'T>) (lengthGenerator: IArgumentGenerator<int>) =
    { new IArgumentGenerator<'T list> with
        member _.Generate(state) =
            let mutable result = []
            let length = lengthGenerator.Generate(&state)
            for _ in 0..length - 1 do
                result <- elementGenerator.Generate(&state) :: result
            result
    }

/// Creates an argument generator that returns arrays containing the values generated by the specified generator.
let array (elementGenerator: IArgumentGenerator<'T>) (lengthGenerator: IArgumentGenerator<int>) =
    { new IArgumentGenerator<'T[]> with
        member _.Generate(state) =
            let length = lengthGenerator.Generate(&state)
            let mutable result = Array.zeroCreate length
            for i in 0..length - 1 do
                result[i] <- elementGenerator.Generate(&state)
            result
    }

/// Creates an argument generator that returns strings containing the char generated by the specified generator.
let string (charGenerator: IArgumentGenerator<char>) (lengthGenerator: IArgumentGenerator<int>) =
    { new IArgumentGenerator<string> with
        member _.Generate(state) =
            let length = lengthGenerator.Generate(&state)
            let stateRef = ref state
            let text = String.Create(length, stateRef, fun span stateRef ->
                let mutable state = stateRef.Value
                for i in 0..span.Length - 1 do
                    span[i] <- charGenerator.Generate(&state)
                stateRef.Value <- state)
            state <- stateRef.Value
            text
    }

/// Creates an argument generator that returns random ascii strings.
let asciiString (lengthGenerator: IArgumentGenerator<int>) : IArgumentGenerator<string> =
    string asciiChar lengthGenerator
