namespace Testexp.Implementations
open Testexp

type TestingBuilder internal () =
    /// for argument (property based test)
    member inline _.Bind((count: int, m: IArgumentGenerator<'T>), [<InlineIfLambda>] f: 'T -> RandomState -> RandomState) : RandomState -> RandomState =
        fun state ->
            let mutable state = state
            for _ in 0..count do
                let struct (next, value) = m.Generate(state)
                state <- f value next
            state

    /// for argument (property based test)
    member inline this.Bind(m: IArgumentGenerator<'T>, [<InlineIfLambda>] f: 'T -> RandomState -> RandomState) : RandomState -> RandomState =
        this.Bind((8, m), f)

    /// for test execution
    member inline _.Bind(testing: ITesting<'T>, [<InlineIfLambda>] f: 'T -> TestContext<'T> -> unit) : RandomState -> RandomState =
        fun state -> testing.Test(f); state

    member inline _.Zero() : RandomState -> RandomState = fun x -> x

    member inline _.Combine([<InlineIfLambda>] a: TestContext<'T> -> unit, [<InlineIfLambda>] b: unit -> TestContext<'T> -> unit) =
        fun (context: TestContext<'T>) -> a context; b () context

    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T) = f

    member inline _.Yield([<InlineIfLambda>] r: TestContext<'T> -> unit) = r

    member inline _.Run([<InlineIfLambda>] f: unit -> RandomState -> RandomState) = ignore <| f () RandomState.Default
