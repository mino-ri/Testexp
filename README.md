# Testexp
Property based testing using computation expression for F#.

## How to use

Testexp helps to write property based testing, but does not have any basic testing features.
Use with other unit testing libraries.
When the test fails, an exception is thrown.

Sample with Xunit:

```fsharp
module Test
open Testexp
open Xunit

[<Fact>]
let simpleStype () =
    test fst ("abc", 2) ==> Assert.equal "abc"

[<Fact>]
let computationExpression () =
    testing {
        let! result = test fst ("abc", 2)
        Assert.equal "abc" result
    }

[<Fact>]
let propertyBasedTest () =
    testing {
        // generate arguments
        let! arg1 = ArgGen.asciiString (ArgGen.intRange 0 16)
        and! arg2 = ArgGen.intRange 0 100
        // execute test
        let! result = test fst (arg1, arg2)
        Assert.equal arg1 result
    }
```
