namespace Testexp.Implementations
open System
open System.Runtime.CompilerServices
open Testexp

type RandomGenerator<'T> = RandomState -> struct(RandomState * 'T)

type Testing =
    static member inline private CreateTestCore([<InlineIfLambda>] execution: unit -> 'T, [<ParamArray>] args: ITuple) =
        { new ITesting<'T> with
            member _.Test(Assertion assertion) =
                let result = try Ok(box (execution())) with ex -> Error(ex)
                let testContext = { Arguments = args; Result = result }
                assertion result testContext

            member _.Test(assertion: 'T -> TestContext<'T> -> unit) =
                let result =
                    try execution()
                    with ex ->
                        let context = { Arguments = args; Result = Error(ex) }
                        raise (ValidationException("Unexpected exception was thrown.", context, ex))
                let context = { Arguments = args; Result = Ok(result) }
                assertion result context

            member _.Test(assertions: ('T -> TestContext<'T> -> unit) list) =
                let result =
                    try execution()
                    with ex ->
                        let context = { Arguments = args; Result = Error(ex) }
                        raise (ValidationException("Unexpected exception was thrown.", context, ex))
                let context = { Arguments = args; Result = Ok(result) }
                for assertion in assertions do
                    assertion result context
        }

    static member CreateTest(f: unit -> 'Result) =
        Testing.CreateTestCore(f, ValueTuple())

    static member CreateTest(f: 'T1 -> 'Result, arg1) =
        let args =
            match box arg1 with
            | :? ITuple as tuple -> tuple
            | x -> Tuple<_>(x)
        Testing.CreateTestCore((fun () -> f arg1), args)

    static member CreateTest(f: 'T1 -> 'T2 -> 'Result, (arg1, arg2)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2), (arg1, arg2))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'Result, (arg1, arg2, arg3)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3), (arg1, arg2, arg3))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'Result, (arg1, arg2, arg3, arg4)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3 arg4), (arg1, arg2, arg3, arg4))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'Result, (arg1, arg2, arg3, arg4, arg5)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3 arg4 arg5), (arg1, arg2, arg3, arg4, arg5))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'Result, (arg1, arg2, arg3, arg4, arg5, arg6)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3 arg4 arg5 arg6), (arg1, arg2, arg3, arg4, arg5, arg6))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'Result, (arg1, arg2, arg3, arg4, arg5, arg6, arg7)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3 arg4 arg5 arg6 arg7), (arg1, arg2, arg3, arg4, arg5, arg6, arg7))

    static member CreateTest(f: 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'T8 -> 'Result, (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) =
        Testing.CreateTestCore((fun () -> f arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8), (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
