namespace Testexp.Implementations
open Testexp

type ArgumentGeneratorBuilder internal () =
    member inline _.Bind(
        m: IArgumentGenerator<'T>,
        [<InlineIfLambda>] f: 'T -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (next, value) = m.Generate(state)
            f value next

    member inline _.Bind2(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        [<InlineIfLambda>] f: 'T1 * 'T2 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            f (value1, value2) state

    member inline _.Bind3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            f (value1, value2, value3) state

    member inline _.Bind4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            f (value1, value2, value3, value4) state

    member inline _.Bind5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            f (value1, value2, value3, value4, value5) state

    member inline _.Bind6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            f (value1, value2, value3, value4, value5, value6) state

    member inline _.Bind7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            let struct (state, value7) = m7.Generate(state)
            f (value1, value2, value3, value4, value5, value6, value7) state

    member inline _.Bind8(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        m8: IArgumentGenerator<'T8>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 -> RandomGenerator<'U>)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            let struct (state, value7) = m7.Generate(state)
            let struct (state, value8) = m8.Generate(state)
            f (value1, value2, value3, value4, value5, value6, value7, value8) state

    member inline _.BindReturn(
        m: IArgumentGenerator<'T>,
        [<InlineIfLambda>] f: 'T -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (next, value) = m.Generate(state)
            next, f value

    member inline _.BindReturn2(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        [<InlineIfLambda>] f: 'T1 * 'T2 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            state, f (value1, value2)

    member inline _.BindReturn3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            state, f (value1, value2, value3)

    member inline _.BindReturn4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            state, f (value1, value2, value3, value4)

    member inline _.BindReturn5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            state, f (value1, value2, value3, value4, value5)

    member inline _.BindReturn6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            state, f (value1, value2, value3, value4, value5, value6)

    member inline _.BindReturn7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            let struct (state, value7) = m7.Generate(state)
            state, f (value1, value2, value3, value4, value5, value6, value7)

    member inline _.BindReturn8(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        m8: IArgumentGenerator<'T8>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 -> 'U)
        : RandomGenerator<'U> =
        fun state ->
            let struct (state, value1) = m1.Generate(state)
            let struct (state, value2) = m2.Generate(state)
            let struct (state, value3) = m3.Generate(state)
            let struct (state, value4) = m4.Generate(state)
            let struct (state, value5) = m5.Generate(state)
            let struct (state, value6) = m6.Generate(state)
            let struct (state, value7) = m7.Generate(state)
            let struct (state, value8) = m8.Generate(state)
            state, f (value1, value2, value3, value4, value5, value6, value7, value8)

    member inline this.MergeSources(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>)
        : RandomGenerator<'T1 * 'T2> =
        this.BindReturn2(m1, m2, id)

    member inline this.MergeSources3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>)
        : RandomGenerator<'T1 * 'T2 * 'T3> =
        this.BindReturn3(m1, m2, m3, id)

    member inline this.MergeSources4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>)
        : RandomGenerator<'T1 * 'T2 * 'T3 * 'T4> =
        this.BindReturn4(m1, m2, m3, m4, id)

    member inline this.MergeSources5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>)
        : RandomGenerator<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        this.BindReturn5(m1, m2, m3, m4, m5, id)

    member inline this.MergeSources6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>)
        : RandomGenerator<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        this.BindReturn6(m1, m2, m3, m4, m5, m6, id)

    member inline this.MergeSources7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>)
        : RandomGenerator<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        this.BindReturn7(m1, m2, m3, m4, m5, m6, m7, id)

    member inline this.MergeSources8(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        m8: IArgumentGenerator<'T8>)
        : RandomGenerator<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        this.BindReturn8(m1, m2, m3, m4, m5, m6, m7, m8, id)

    member inline _.Return(value: 'T) : RandomGenerator<'T> = fun state -> state, value

    member inline _.Zero() : RandomGenerator<unit> = fun state -> state, ()

    member inline _.Delay([<InlineIfLambda>] f: unit -> RandomGenerator<'T>) = f

    member inline _.Combine([<InlineIfLambda>] m1: RandomGenerator<unit>, [<InlineIfLambda>] m2: unit -> RandomGenerator<'T>) =
        fun state ->
            let struct (next, _) = m1 state
            m2 () next

    member inline _.For(source: seq<'T>, [<InlineIfLambda>] f: 'T -> RandomGenerator<unit>) : RandomGenerator<unit> =
        fun state ->
            let mutable state = state
            for item in source do
                let struct (next, _) = f item state
                state <- next
            struct (state, ())

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, body: unit -> RandomGenerator<unit>) : RandomGenerator<unit> =
        fun state ->
            let mutable state = state
            while condition () do
                let struct (next, _) = body () state
                state <- next
            struct (state, ())

    member inline _.Run([<InlineIfLambda>] f: unit -> RandomGenerator<'T>) =
        { new IArgumentGenerator<'T> with
            member _.Generate(state) = f () state
        }
