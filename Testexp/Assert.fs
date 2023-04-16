module Testexp.Assert

/// Fails the test.
let fail message (context: TestContext<'Result>) : unit = raise (ValidationException(message, context))

/// Verifies that the exact exception is thrown.
let thrown<'Exn when 'Exn :> exn> : Assertion =
    Assertion(fun result context ->
        match result with
        | Ok _ -> fail ($"Exception {typeof<'Exn>.Name} should be thrown.") context
        | Error(:? 'Exn as _) -> ()
        | Error(ex) -> fail ($"Exception {typeof<'Exn>.Name} should be thrown.\r\nACTUAL  : %s{ex.GetType().Name}\r\n%A{ex}") context
    )

/// Verifies that the condition is ture.
let isTrue (condition: bool) (context: TestContext<'Result>) =
    if not condition then
        fail "should be true." context

/// Verifies that the condition is false.
let isFalse (condition: bool) (context: TestContext<'Result>) =
    if condition then
        fail "should be false." context

/// Verifies that the value satisfies the specified condition.
let should (predicate: 'T -> bool) (formatError: 'T -> string) (actual: 'T) (context: TestContext<'Result>) =
    if not (predicate actual) then
        fail (formatError actual) context

/// Verifies that the value does not satisfy the specified condition.
let shouldNot (predicate: 'T -> bool) (formatError: 'T -> string) (actual: 'T) (context: TestContext<'Result>) =
    if predicate actual then
        fail (formatError actual) context

let private formatValue value =
    match box value with
    | null -> "<null>"
    | value -> sprintf "%A" value

/// Formats a error explanation.
let formatError explain (actual: 'T) = sprintf "%s\r\nACTUAL  : %s" explain (formatValue actual)

/// Formats a error explanation.
let formatError2 explain (expected: 'T1) (actual: 'T2) =
    sprintf "%s\r\nEXPECTED: %s\r\nACTUAL  : %s" explain (formatValue expected) (formatValue actual)

/// Verifies that two values are equal.
let equal (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((=) expected) (formatError2 "Should be EXPECTED = ACTUAL." expected) actual context

/// Verifies that two values are not equal.
let notEqual (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((<>) expected) (formatError2 "Should be EXPECTED <> ACTUAL." expected) actual context

/// Verifies that the actual value is greater than the expected value.
let greaterThan (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((<) expected) (formatError2 "Should be EXPECTED < ACTUAL." expected) actual context

/// Verifies that the actual value is less than the expected value.
let lessThan (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((>) expected) (formatError2 "Should be EXPECTED > ACTUAL." expected) actual context

/// Verifies that the actual value is greater than or equal to the expected value.
let greaterThanOrEqual (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((<=) expected) (formatError2 "Should be EXPECTED <= ACTUAL." expected) actual context

/// Verifies that the actual value is less than or equal to the expected value.
let lessThanOrEqual (expected: 'T) (actual: 'T) (context: TestContext<'Result>) =
    should ((>=) expected) (formatError2 "Should be EXPECTED >= ACTUAL." expected) actual context

/// Verifies that the sequence contains the expected value.
let contains (expected: 'T) (actual: seq<'T>) (context: TestContext<'Result>) =
    should (Seq.contains expected) (formatError2 "Should contain EXPECTED." expected) actual context

/// Verifies that the sequence does not contain the expected value.
let notContains (expected: 'T) (actual: seq<'T>) (context: TestContext<'Result>) =
    shouldNot (Seq.contains expected) (formatError2 "Should not contain EXPECTED." expected) actual context

/// Verifies that the string contains the expected substring.
let hasSubstring (substring: string) (actual: string) (context: TestContext<'Result>) =
    should (fun (s: string) -> s.Contains(substring)) (formatError2 "Should contain EXPECTED." substring) actual context

/// Verifies that the string starts with the expected substring.
let startsWith (substring: string) (actual: string) (context: TestContext<'Result>) =
    should (fun (s: string) -> s.StartsWith(substring)) (formatError2 "Should start with EXPECTED." substring) actual context

/// Verifies that the string ends with the expected substring.
let endsWith (substring: string) (actual: string) (context: TestContext<'Result>) =
    should (fun (s: string) -> s.EndsWith(substring)) (formatError2 "Should end with EXPECTED." substring) actual context

/// Verifies that the value is not null.
let notNull (actual: 'T when 'T: null) (context: TestContext<'Result>) = shouldNot isNull (formatError "Should be not null.") actual context

/// Verifies that the value is null.
let isNull (actual: 'T when 'T: null) (context: TestContext<'Result>) = should isNull (formatError "Should be null.") actual context

/// Verifies that the option is Some.
let some (actual: Option<'T>) (context: TestContext<'Result>) = should Option.isSome (formatError "Should be Some.") actual context

/// Verifies that the option is None.
let none (actual: Option<'T>) (context: TestContext<'Result>) = should Option.isNone (formatError "Should be None.") actual context

/// Verifies that the value option is ValueSome.
let vsome (actual: ValueOption<'T>) (context: TestContext<'Result>) =
    should ValueOption.isSome (formatError "Should be ValueSome.") actual context

/// Verifies that the value option is ValueNone.
let vnone (actual: ValueOption<'T>) (context: TestContext<'Result>) =
    should ValueOption.isNone (formatError "Should be ValueNone.") actual context

/// Verifies that the result is Ok.
let ok (actual: Result<'T, 'U>) (context: TestContext<'Result>) =
    should
        (function
        | Ok _ -> true
        | Error _ -> false)
        (formatError "Should be Ok.")
        actual
        context

/// Verifies that the result is Error.
let error (actual: Result<'T, 'U>) (context: TestContext<'Result>) =
    should
        (function
        | Ok _ -> false
        | Error _ -> true)
        (formatError "Should be Error.")
        actual
        context
