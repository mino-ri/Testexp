module Testexp.ArgGen
open System

type private DefaultArgumentGenerator = DefaultArgumentGenerator with
    interface IArgumentGenerator<uint64> with
        member _.Generate(RandomState seed) = 
            let value = seed * 2147483649uL + 273051931uL
            struct (RandomState(value), value)


type private ForEachArgumentGenerator<'T>(args: 'T[]) =
    let mutable index = -1
    interface IArgumentGenerator<'T> with
        member _.Generate(state) = 
            index <- min (args.Length - 1) (index + 1)
            state, args[index]


type private ListArgumentGenerator<'T>(elementGenerator: IArgumentGenerator<'T>, lengthGenerator: IArgumentGenerator<int>) =
    interface IArgumentGenerator<'T list> with
        member _.Generate(state) =
            let rec recSelf length lst state =
                if length = 0 then
                    struct (state, lst)
                else
                    let struct (state, value) = elementGenerator.Generate(state)
                    recSelf (length - 1) (value :: lst) state
            let struct (state, length) = lengthGenerator.Generate(state)
            recSelf length [] state


type private ArrayArgumentGenerator<'T>(elementGenerator: IArgumentGenerator<'T>, lengthGenerator: IArgumentGenerator<int>) =
    interface IArgumentGenerator<'T[]> with
        member _.Generate(state) =
            let struct (state, length) = lengthGenerator.Generate(state)
            let result = Array.zeroCreate length
            let mutable state = state
            for i in 0..length - 1 do
                let struct (next, value) = elementGenerator.Generate(state)
                result[i] <- value
                state <- next
            state, result


type private StringArgumentGenerator(elementGenerator: IArgumentGenerator<char>, lengthGenerator: IArgumentGenerator<int>) =
    interface IArgumentGenerator<string> with
        member _.Generate(state) =
            let struct (state, length) = lengthGenerator.Generate(state)
            let state = ref state
            let text = String.Create(length, state, fun span state ->
                for i in 0..span.Length - 1 do
                    let struct (next, value) = elementGenerator.Generate(state.Value)
                    span[i] <- value
                    state.Value <- next)
            state.Value, text


/// Gets an argument generator that returns pseudo-random numbers. This generates the same numbers in the same order each time.
let random = DefaultArgumentGenerator :> IArgumentGenerator<uint64>

/// Creates an argument generator that returns the constant value.
let constant (value: 'T) =
    { new IArgumentGenerator<'T> with
        member _.Generate(state) = state, value
    }

/// Represents the monad bind operator for IArgumentGenerator<T>. This is an inline function.
let inline bindInline ([<InlineIfLambda>] binding: 'T -> IArgumentGenerator<'U>) (source: IArgumentGenerator<'T>) : IArgumentGenerator<'U> =
    { new IArgumentGenerator<'U> with
        member _.Generate(state) =
            let struct (state, value) = source.Generate(state)
            (binding value).Generate(state)
    }

/// Represents the monad bind operator for IArgumentGenerator<T>.
let bind (binding: 'T -> IArgumentGenerator<'U>) source = bindInline binding source

/// Represents the functor map operator for IArgumentGenerator<T>. This is an inline function.
let inline mapInline ([<InlineIfLambda>] mapping: 'T -> 'U) (source: IArgumentGenerator<'T>) : IArgumentGenerator<'U> =
    { new IArgumentGenerator<'U> with
          member _.Generate(state) =
              let struct (state, value) = source.Generate(state)
              state, mapping value
    }

/// Represents the functor map operator for IArgumentGenerator<T>.
let map (mapping: 'T -> 'U) source = mapInline mapping source
    
/// Creates an argument generator from the generator function. This is an inline function.
let inline generatorInline ([<InlineIfLambda>] mapping: uint64 -> 'T) = mapInline mapping random

/// Creates an argument generator from the generator function.
let generator (mapping: uint64 -> 'T) = mapInline mapping random

/// Creates an argument generator that returns random integers in the specified range.
let sbyteRange (minInclusive: sbyte) (maxExclusive: sbyte) =
    generatorInline (fun value -> byte value % byte (maxExclusive - minInclusive) + byte minInclusive |> sbyte)

/// Creates an argument generator that returns random integers in the specified range.
let int16Range (minInclusive: int16) (maxExclusive: int16) =
    generatorInline (fun value -> uint16 value % uint16 (maxExclusive - minInclusive) + uint16 minInclusive |> int16)

/// Creates an argument generator that returns random integers in the specified range.
let int32Range (minInclusive: int32) (maxExclusive: int32) =
    generatorInline (fun value -> uint32 value % uint32 (maxExclusive - minInclusive) + uint32 minInclusive |> int32)

/// Creates an argument generator that returns random integers in the specified range.
let int64Range (minInclusive: int64) (maxExclusive: int64) =
    generatorInline (fun value -> value % uint64 (maxExclusive - minInclusive) + uint64 minInclusive |> int64)

/// Creates an argument generator that returns random integers in the specified range.
let byteRange minInclusive maxExclusive =
    generatorInline (fun value -> byte value % (maxExclusive - minInclusive) + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint16Range minInclusive maxExclusive =
    generatorInline (fun value -> uint16 value % (maxExclusive - minInclusive) + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint32Range minInclusive maxExclusive =
    generatorInline (fun value -> uint32 value % (maxExclusive - minInclusive) + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let uint64Range minInclusive maxExclusive =
    generatorInline (fun value -> value % (maxExclusive - minInclusive) + minInclusive)

/// Creates an argument generator that returns random integers in the specified range.
let intRange (minInclusive: int) (maxExclusive: int) =
    generatorInline (fun value -> uint value % uint (maxExclusive - minInclusive) + uint minInclusive |> int)

/// Creates an argument generator that returns random integers in the specified range.
let uintRange minInclusive maxExclusive =
    generatorInline (fun value -> uint value % (maxExclusive - minInclusive) + minInclusive)

/// Creates an argument generator that returns random chars in the specified range.
let charRange (minInclusive: char) (maxExclusive: char) =
    generatorInline (fun value -> uint value % (uint maxExclusive - uint minInclusive) + uint minInclusive |> char)

/// Gets an argument generator that returns any sbyte value.
let sbyte = generatorInline sbyte

/// Gets an argument generator that returns any int16 value.
let int16 = generatorInline int16

/// Gets an argument generator that returns any int32 value.
let int32 = generatorInline int32

/// Gets an argument generator that returns any int64 value.
let int64 = generatorInline int64

/// Gets an argument generator that returns any byte value.
let byte = generatorInline byte

/// Gets an argument generator that returns any uint16 value.
let uint16 = generatorInline uint16

/// Gets an argument generator that returns any uint32 value.
let uint32 = generatorInline uint32

/// Gets an argument generator that returns any uint64 value.
let uint64 = generatorInline uint64

/// Gets an argument generator that returns any int value.
let int = generatorInline int

/// Gets an argument generator that returns any uint value.
let uint = generatorInline uint

/// Creates an argument generator that returns random ascii chars (0x20 - 0x7E).
let asciiChar = charRange ' ' '\x7F'

/// Creates an argument generator that returns random floating point numbers in the specified range.
let floatRange (minimum: float) (maximum: float) =
    let range = maximum - minimum
    generatorInline (fun value ->
        let value = float (value % 2147483648uL) / 2147483648.0 * range + minimum
        value |> min maximum |> max minimum)

/// Creates an argument generator that returns each items.
let forEach (items: 'T[]) = items.Length, ForEachArgumentGenerator(items) :> IArgumentGenerator<'T>

/// Creates an argument generator that returns lists containing the values generated by the specified generator.
let list (elementGenerator: IArgumentGenerator<'T>) (lengthGenerator: IArgumentGenerator<int>) : IArgumentGenerator<'T list> =
    upcast ListArgumentGenerator(elementGenerator, lengthGenerator)

/// Creates an argument generator that returns arrays containing the values generated by the specified generator.
let array (elementGenerator: IArgumentGenerator<'T>) (lengthGenerator: IArgumentGenerator<int>) : IArgumentGenerator<'T[]> =
    upcast ArrayArgumentGenerator(elementGenerator, lengthGenerator)

/// Creates an argument generator that returns strings containing the char generated by the specified generator.
let string (charGenerator: IArgumentGenerator<char>) (lengthGenerator: IArgumentGenerator<int>) : IArgumentGenerator<string> =
    upcast StringArgumentGenerator(charGenerator, lengthGenerator)

/// Creates an argument generator that returns random ascii strings.
let asciiString (lengthGenerator: IArgumentGenerator<int>) : IArgumentGenerator<string> =
    upcast StringArgumentGenerator(asciiChar, lengthGenerator)
