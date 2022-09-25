namespace Testexp.Implementations
open Testexp

type TestExecution = delegate of byref<RandomState> -> unit

type TestingBuilder internal () =
    /// for argument (property based test)
    member inline _.Bind((count: int, m: IArgumentGenerator<'T>), [<InlineIfLambda>] f: 'T -> TestExecution) =
        TestExecution(fun state ->
            let mutable state = state
            for _ in 0..count do
                (f (m.Generate(&state))).Invoke(&state))

    /// for argument (property based test)
    member inline this.Bind(m: IArgumentGenerator<'T>, [<InlineIfLambda>] f) =
        this.Bind((8, m), f)

    /// for test execution
    member inline _.Bind(testing: ITesting<'T>, [<InlineIfLambda>] f: 'T -> TestContext<'T> -> unit) =
        TestExecution(fun _ -> testing.Test(f))

    member inline _.Zero() = TestExecution(fun _ -> ())

    member inline _.Combine([<InlineIfLambda>] a: TestContext<'T> -> unit, [<InlineIfLambda>] b: unit -> TestContext<'T> -> unit) =
        fun (context: TestContext<'T>) -> a context; b () context

    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T) = f

    member inline _.Yield([<InlineIfLambda>] r: TestContext<'T> -> unit) = r

    member inline _.Run([<InlineIfLambda>] f: unit -> TestExecution) =
        let mutable state = RandomState.Default
        (f ()).Invoke(&state)
