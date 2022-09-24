module Testexp.Test.Sample
open Testexp
open Xunit

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
let sbyteRange () =
    testing {
        let! x = 1024, ArgGen.sbyteRange 0y 32y
        test id x ==> [
            Assert.greaterThanOrEqual 0y
            Assert.lessThan 32y
        ]
    }

[<Fact>]
let int16Range () =
    testing {
        let! x = 1024, ArgGen.int16Range 0s 32s
        test id x ==> [
            Assert.greaterThanOrEqual 0s
            Assert.lessThan 32s
        ]
    }

[<Fact>]
let int32Range () =
    testing {
        let! x = 1024, ArgGen.int32Range 0 32
        test id x ==> [
            Assert.greaterThanOrEqual 0
            Assert.lessThan 32
        ]
    }
    
[<Fact>]
let int64Range () =
    testing {
        let! x = 1024, ArgGen.int64Range 0L 32L
        test id x ==> [
            Assert.greaterThanOrEqual 0L
            Assert.lessThan 32L
        ]
    }

[<Fact>]
let intRange () =
    testing {
        let! x = 1024, ArgGen.intRange 0 32
        test id x ==> [
            Assert.greaterThanOrEqual 0
            Assert.lessThan 32
        ]
    }

[<Fact>]
let byteRange () =
    testing {
        let! x = 1024, ArgGen.byteRange 0uy 32uy
        test id x ==> [
            Assert.greaterThanOrEqual 0uy
            Assert.lessThan 32uy
        ]
    }

[<Fact>]
let uint16Range () =
    testing {
        let! x = 1024, ArgGen.uint16Range 0us 32us
        test id x ==> [
            Assert.greaterThanOrEqual 0us
            Assert.lessThan 32us
        ]
    }

[<Fact>]
let uint32Range () =
    testing {
        let! x = 1024, ArgGen.uint32Range 0u 32u
        test id x ==> [
            Assert.greaterThanOrEqual 0u
            Assert.lessThan 32u
        ]
    }
    
[<Fact>]
let uint64Range () =
    testing {
        let! x = 1024, ArgGen.uint64Range 0uL 32uL
        test id x ==> [
            Assert.greaterThanOrEqual 0uL
            Assert.lessThan 32uL
        ]
    }

[<Fact>]
let uintRange () =
    testing {
        let! x = 1024, ArgGen.uintRange 0u 32u
        test id x ==> [
            Assert.greaterThanOrEqual 0u
            Assert.lessThan 32u
        ]
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
