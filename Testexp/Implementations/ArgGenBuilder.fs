namespace Testexp.Implementations
open Testexp

type ArgGenBuilder internal () =
    member inline _.Bind(
        m: IArgumentGenerator<'T>,
        [<InlineIfLambda>] f: 'T -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state -> (f (m.Generate(&state))).Invoke(&state))

    member inline _.Bind2(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        [<InlineIfLambda>] f: 'T1 * 'T2 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            (f (value1, value2)).Invoke(&state))

    member inline _.Bind3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            (f (value1, value2, value3)).Invoke(&state))

    member inline _.Bind4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            (f (value1, value2, value3, value4)).Invoke(&state))

    member inline _.Bind5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            (f (value1, value2, value3, value4, value5)).Invoke(&state))

    member inline _.Bind6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            (f (value1, value2, value3, value4, value5, value6)).Invoke(&state))

    member inline _.Bind7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            let value7 = m7.Generate(&state)
            (f (value1, value2, value3, value4, value5, value6, value7)).Invoke(&state))

    member inline _.Bind8(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        m8: IArgumentGenerator<'T8>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 -> ArgGenCode<'U>)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            let value7 = m7.Generate(&state)
            let value8 = m8.Generate(&state)
            (f (value1, value2, value3, value4, value5, value6, value7, value8)).Invoke(&state))

    member inline _.BindReturn(
        [<InlineIfLambda>] m: ArgGenCode<'T>,
        [<InlineIfLambda>] f: 'T -> 'U)
        =
        ArgGenCode<'U>(fun state -> f (m.Invoke(&state)))

    member inline _.BindReturn2(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        [<InlineIfLambda>] f: 'T1 * 'T2 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            f (value1, value2))

    member inline _.BindReturn3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            f (value1, value2, value3))

    member inline _.BindReturn4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            f (value1, value2, value3, value4))

    member inline _.BindReturn5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            f (value1, value2, value3, value4, value5))

    member inline _.BindReturn6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            f (value1, value2, value3, value4, value5, value6))

    member inline _.BindReturn7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>,
        [<InlineIfLambda>] f: 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'U)
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            let value7 = m7.Generate(&state)
            f (value1, value2, value3, value4, value5, value6, value7))

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
        =
        ArgGenCode<'U>(fun state ->
            let value1 = m1.Generate(&state)
            let value2 = m2.Generate(&state)
            let value3 = m3.Generate(&state)
            let value4 = m4.Generate(&state)
            let value5 = m5.Generate(&state)
            let value6 = m6.Generate(&state)
            let value7 = m7.Generate(&state)
            let value8 = m8.Generate(&state)
            f (value1, value2, value3, value4, value5, value6, value7, value8))

    member inline this.MergeSources(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>)
        : ArgGenCode<'T1 * 'T2> =
        this.BindReturn2(m1, m2, id)

    member inline this.MergeSources3(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>)
        : ArgGenCode<'T1 * 'T2 * 'T3> =
        this.BindReturn3(m1, m2, m3, id)

    member inline this.MergeSources4(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>)
        : ArgGenCode<'T1 * 'T2 * 'T3 * 'T4> =
        this.BindReturn4(m1, m2, m3, m4, id)

    member inline this.MergeSources5(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>)
        : ArgGenCode<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        this.BindReturn5(m1, m2, m3, m4, m5, id)

    member inline this.MergeSources6(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>)
        : ArgGenCode<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        this.BindReturn6(m1, m2, m3, m4, m5, m6, id)

    member inline this.MergeSources7(
        m1: IArgumentGenerator<'T1>,
        m2: IArgumentGenerator<'T2>,
        m3: IArgumentGenerator<'T3>,
        m4: IArgumentGenerator<'T4>,
        m5: IArgumentGenerator<'T5>,
        m6: IArgumentGenerator<'T6>,
        m7: IArgumentGenerator<'T7>)
        : ArgGenCode<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
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
        : ArgGenCode<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        this.BindReturn8(m1, m2, m3, m4, m5, m6, m7, m8, id)

    member inline _.Return(value: 'T) =
        ArgGenCode<'T>(fun _ -> value)
    
    member inline _.ReturnFrom(m: IArgumentGenerator<'T>) =
        ArgGenCode<'T>(fun state -> m.Generate(&state))

    member inline _.Zero() =
        ArgGenCode<unit>(fun _ -> ())

    member inline _.Delay([<InlineIfLambda>] f: unit -> ArgGenCode<'T>) =
        ArgGenCode<'T>(fun state -> (f ()).Invoke(&state))

    member inline _.Combine([<InlineIfLambda>] m1: ArgGenCode<unit>, [<InlineIfLambda>] m2: ArgGenCode<'T>) =
        ArgGenCode<'T>(fun state ->
            m1.Invoke(&state)
            m2.Invoke(&state))

    member inline _.For(source: seq<'T>, [<InlineIfLambda>] body: 'T -> ArgGenCode<unit>) : ArgGenCode<unit> =
        ArgGenCode<unit>(fun state -> for item in source do (body item).Invoke(&state))

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, [<InlineIfLambda>] body: ArgGenCode<unit>) : ArgGenCode<unit> =
        ArgGenCode<unit>(fun state -> while condition () do body.Invoke(&state))

    member inline _.Run([<InlineIfLambda>] f: ArgGenCode<'T>) =
        { new IArgumentGenerator<'T> with
            member _.Generate(state) = f.Invoke(&state)
        }
