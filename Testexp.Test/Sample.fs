module Testexp.Test.Sample
open Testexp
open Xunit

let private isRandom<'T, 'Result> count : 'T -> TestContext<'Result> -> unit =
    let hashSet = System.Collections.Generic.HashSet<'T>()
    let mutable overlapCount = 0
    let mutable callCount = 0
    let predicate actual =
        if not (hashSet.Add(actual)) then
            overlapCount <- overlapCount + 1
        callCount <- callCount + 1
        overlapCount < count
    Assert.should predicate (fun act -> Assert.formatError (sprintf "ACTUAL values are probably not random.\r\n%d/%d %A" hashSet.Count callCount (hashSet |> Seq.toList)) act)

[<Fact>]
let testRandom () =
    let random = isRandom 5
    let f values context =
        for v in values do
            random v context
    let executeTest (values: int list) = test id values ==> f
    executeTest [ 1; 2; 3; 4; 5 ]
    test executeTest [ 1; 1; 1; 1; 1 ] ==> Assert.thrown<ValidationExceptionBase>

[<Fact>]
let tuple2 () =
    test Target.tuple1of2 ("abc", 12) ==> Assert.equal "abc"

[<Fact>]
let tuple3 () =
    test Target.tuple1of3 ("abc", 12, 13) ==> Assert.equal "abc"

[<Fact>]
let tuple4 () =
    test Target.tuple1of4 ("abc", 12, 13, 14) ==> Assert.equal "abc"

[<Fact>]
let tuple5 () =
    test Target.tuple1of5 ("abc", 12, 13, 14, 15) ==> Assert.equal "abc"

[<Fact>]
let tuple6 () =
    test Target.tuple1of6 ("abc", 12, 13, 14, 15, 16) ==> Assert.equal "abc"

[<Fact>]
let tuple7 () =
    test Target.tuple1of7 ("abc", 12, 13, 14, 15, 16, 17) ==> Assert.equal "abc"

[<Fact>]
let tuple8 () =
    test Target.tuple1of8 ("abc", 12, 13, 14, 15, 16, 17, 18) ==> Assert.equal "abc"
    
[<Fact>]
let curry2 () =
    test Target.curry1of2 ("abc", 12) ==> Assert.equal "abc"

[<Fact>]
let curry3 () =
    test Target.curry1of3 ("abc", 12, 13) ==> Assert.equal "abc"

[<Fact>]
let curry4 () =
    test Target.curry1of4 ("abc", 12, 13, 14) ==> Assert.equal "abc"

[<Fact>]
let curry5 () =
    test Target.curry1of5 ("abc", 12, 13, 14, 15) ==> Assert.equal "abc"

[<Fact>]
let curry6 () =
    test Target.curry1of6 ("abc", 12, 13, 14, 15, 16) ==> Assert.equal "abc"

[<Fact>]
let curry7 () =
    test Target.curry1of7 ("abc", 12, 13, 14, 15, 16, 17) ==> Assert.equal "abc"

[<Fact>]
let curry8 () =
    test Target.curry1of8 ("abc", 12, 13, 14, 15, 16, 17, 18) ==> Assert.equal "abc"
    
[<Fact>]
let baseGenerator () =
    let random = isRandom 5
    testing {
        let! x = 10000, ArgGen.random
        test id x ==> [
            random
        ]
    }

[<Fact>]
let sbyteRange () =
    let random = isRandom 32
    testing {
        let! x = 32, ArgGen.sbyteRange 0y 127y
        test id x ==> [
            Assert.greaterThanOrEqual 0y
            Assert.lessThan 127y
            random
        ]
    }

[<Fact>]
let int16Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int16Range 0s 30001s
        test id x ==> [
            Assert.greaterThanOrEqual 0s
            Assert.lessThan 30000s
            random
        ]
    }

[<Fact>]
let int32Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int32Range 0 30001
        test id x ==> [
            Assert.greaterThanOrEqual 0
            Assert.lessThan 30001
            random
        ]
    }
    
[<Fact>]
let int64Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int64Range 0L 30001L
        test id x ==> [
            Assert.greaterThanOrEqual 0L
            Assert.lessThan 30001L
            random
        ]
    }

[<Fact>]
let intRange () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.intRange 0 30001
        test id x ==> [
            Assert.greaterThanOrEqual 0
            Assert.lessThan 30001
            random
        ]
    }

[<Fact>]
let byteRange () =
    let random = isRandom 10
    testing {
        let! x = 32, ArgGen.byteRange 0uy 128uy
        test id x ==> [
            Assert.greaterThanOrEqual 0uy
            Assert.lessThan 128uy
            random
        ]
    }

[<Fact>]
let uint16Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint16Range 0us 30001us
        test id x ==> [
            Assert.greaterThanOrEqual 0us
            Assert.lessThan 30001us
            random
        ]
    }

[<Fact>]
let uint32Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint32Range 0u 30001u
        test id x ==> [
            Assert.greaterThanOrEqual 0u
            Assert.lessThan 30001u
            random
        ]
    }
    
[<Fact>]
let uint64Range () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint64Range 0uL 30001uL
        test id x ==> [
            Assert.greaterThanOrEqual 0uL
            Assert.lessThan 30001uL
            random
        ]
    }

[<Fact>]
let uintRange () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uintRange 0u 30001u
        test id x ==> [
            Assert.greaterThanOrEqual 0u
            Assert.lessThan 30001u
            random
        ]
    }

[<Fact>]
let floatRange () =
    let random = isRandom 5
    testing {
        let! x = 1024, ArgGen.floatRange -1.0 1.0
        test id x ==> [
            Assert.greaterThanOrEqual -1.0
            Assert.lessThanOrEqual 1.0
            random
        ]
    }

[<Fact>]
let sbyteAny () =
    let random = isRandom 10
    testing {
        let! x = 32, ArgGen.sbyte
        test id x ==> random
    }

[<Fact>]
let int16Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int16
        test id x ==> random
    }

[<Fact>]
let int32Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int32
        test id x ==> random
    }

[<Fact>]
let int64Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.int64
        test id x ==> random
    }

[<Fact>]
let byteAny () =
    let random = isRandom 10
    testing {
        let! x = 32, ArgGen.byte
        test id x ==> random
    }

[<Fact>]
let uint16Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint16
        test id x ==> random
    }

[<Fact>]
let uint32Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint32
        test id x ==> random
    }

[<Fact>]
let uint64Any () =
    let random = isRandom 10
    testing {
        let! x = 1024, ArgGen.uint64
        test id x ==> random
    }

[<Fact>]
let propertyBasedTest () =
    testing {
        let! x = ArgGen.asciiString (ArgGen.intRange 0 32)
        let! y = ArgGen.byte
        test Target.tuple1of2 (x, y) ==> Assert.equal x
    }

[<Fact>]
let arrayLength () =
    testing {
        let! lst = ArgGen.array (ArgGen.byte) (ArgGen.intRange 4 32)
        let! result = test id lst
        Assert.greaterThanOrEqual 4 result.Length
        Assert.lessThan 32 result.Length
    }

[<Fact>]
let listLength () =
    testing {
        let! lst = ArgGen.list (ArgGen.byte) (ArgGen.intRange 4 32)
        let! result = test id lst
        Assert.greaterThanOrEqual 4 result.Length
        Assert.lessThan 32 result.Length
    }

[<Fact>]
let ok () =
    let f (value: Result<unit, unit>) : unit = test id value ==> Assert.ok
    f (Ok())
    test f (Error()) ==> Assert.thrown<ValidationExceptionBase>


[<Fact>]
let error () =
    let f (value: Result<unit, unit>) : unit = test id value ==> Assert.error
    f (Error())
    test f (Ok()) ==> Assert.thrown<ValidationExceptionBase>

[<Fact>]
let some () =
    let f (value: unit option) : unit = test id value ==> Assert.some
    f (Some())
    test f None ==> Assert.thrown<ValidationExceptionBase>

[<Fact>]
let none () =
    let f (value: unit option) : unit = test id value ==> Assert.none
    f None
    test f (Some()) ==> Assert.thrown<ValidationExceptionBase>

[<Fact>]
let vsome () =
    let f (value: unit voption) : unit = test id value ==> Assert.vsome
    f (ValueSome())
    test f ValueNone ==> Assert.thrown<ValidationExceptionBase>

[<Fact>]
let vnone () =
    let f (value: unit voption) : unit = test id value ==> Assert.vnone
    f ValueNone
    test f (ValueSome()) ==> Assert.thrown<ValidationExceptionBase>
