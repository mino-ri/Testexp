# Testexp
Property based testing for F#.

`Testexp` helps to write property based testing, but does not have any testing integrations.
Use with other unit testing libraries.
When the assertion have failed, an exception is thrown.

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
        // execute the target function and get the return value
        let! result = test fst (arg1, arg2)
        // assert
        Assert.equal arg1 result
    }
```

## Getting Started

Open `Testexp` namespace.

```fsharp
open Testexp
```

To test, write code like this:

```fsharp
test (*targetFunction*) (*arguments*) ==> (*assertion(s)*)
```

or

```fsharp
testing {
    let! result = test (*targetFunction*) (*arguments*)
    // assertion
    // assertion
    // ...
}
```

### `test` operator

```fsharp
val test : testFunc: #('Arg -> 'Result) -> args: 'Arg -> ITesting<'Result>
```

`test` makes a `ITesting<'T>` from a function that is the target of the test.
`testFunc` is a function, that can be curreied-style either tuple-style. `args` is the arguments applied to the `testFunc`, that is always tuple-style.  
Both the following codes are valid:

```fsharp
let curriedPlus (x: int) (y: int) = x + y
test curriedPlus (2, 3)
```

```fsharp
let tuplePlus (x: int, y: int) = x + y
test tuplePlus (2, 3)
```

### Assertions and `==>` operator

To verify the return values, use `Testexp.Assert` module. like this:

```fsharp
test curriedPlus (2, 3) ==> Assert.equal 5
```

The `==>` operator pipes the return value from `ITesting<'T>` to assertions. For multiple assertions, write an assertion *list* in the right operand.

```fsharp
test curriedPlus (2, 3) ==> [
     Assert.equal 5
     Assert.notEqual 1
]
```

If you need to modify the return value, use the `testing` computation expression instead of the `==>` operator.

```fsharp
testing {
    let! result = test curriedPlus (2, 3)
    let negated = -result
    Assert.equal -5 negated
    Assert.notEqual 0 negated
}
```

The first parameter of the assertion functions is the *EXPECTED* value and the second parameter is the *ACTUAL* value. When using the `==>` operator, the second argument is omitted (and the assertion function is curried).

## Argument Generator

To generate random arguments, use `Testexp.ArgGen` module and `argGen` computation expression.

For example, the following is check that the sum of two positive numbers is greater than either of them.

```fsharp
testing {
    let! x = ArgGen.intRange 1 10000
    let! y = ArgGen.intRange 1 10000
    test curriedPlus (x, y) ==> [
        Assert.greaterThan x
        Assert.greaterThan y
    ]
}
```

`ArgGen.intRange` generates random `int` values in the specified range.

By default, each argument generator generates 64 values. To specify the number of generations, write an `int` before the argument generator.

```fsharp
testing {
    let! x = 1000, ArgGen.intRange 1 10000
    let! y = 1000, ArgGen.intRange 1 10000
    test curriedPlus (x, y) ==> [
        Assert.greaterThan x
        Assert.greaterThan y
    ]
}
```

### Builtin Argument Generators

| Value/Function and Signature | Description |
| --- | --- |
| `intX` | Generates random `intX` numbers. (for example, `int8` , `int16` and so on)  |
| `uintX` | Generates random `uintX` numbers. (for example, `uint8` , `uint16` and so on)  |
| `intXRange minInclusive maxExclusive` | Generates random `intX` numbers in the specified range. |
| `uintXRange minInclusive maxExclusive` | Generates random `uintX` numbers in the specified range. |
| `floatRamge minimum maximum` | Generates random `float` numbers in the specified range. |
| `charRange minInclusive maxExclusive` | Generates random `char` in the specified range. |
| `asciiChar` | Generates random `char` in `0X20` - `0x7E`. |
| `list elementGenerator lengthGenerator` | Generates lists containing the values generated by `elementGenerator` . |
| `array elementGenerator lengthGenerator` | Generates arrays containing the values generated by `elementGenerator` . |
| `string charGenerator lengthGenerator` | Generates strings containing the chars generated by `charGenerator` . |
| `asciiString lengthGenerator` | Generates strings containing ASCII char(same as `asciiChar` generates). |
| `constant value` | Returns `value` always. |
| `forEach items` | Returns the elements of `items` in order. |

### Custom Argument Generator

To build custom argument generators, use `argGen` computation expression and/or monad operators in `Testexp.ArgGen` module. For example, the following is argument generator that generates random square numbers.

```fsharp
let squareRange min max =
    argGen {
        let! x = ArgGen.intRange min max
        return x * x
    }
```

or

```fsharp
let squareRange min max =
    ArgGen.intRange min max
    |> ArgGen.map (fun x -> x * x)
```

`ArgGen` supports `map`, `filter`, `bind`, and `choose`.

### Determinism

The argument generators is *deterministic*.
The same codes always generates the same values.
