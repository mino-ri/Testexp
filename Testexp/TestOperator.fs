[<AutoOpen>]
module Testexp.TestOperators
open Testexp.Implementations

/// Creates ITesting from curried-style or tuple-style function.
let inline test (testFunc: ^``('Args -> 'Result)``) (args: ^Args) : ITesting< ^Result > =
    Testexp.Implementations.Operators.createTest (Unchecked.defaultof<Testing>) testFunc args

/// Pipes the result value from ITesting to assersion.
let inline (==>) (test: ^``ITesting<'T>``) (assertion: ^Assertion) =
    (^``ITesting<'T>`` : (member Test : ^Assertion -> unit) test, assertion)

/// Builds a argument generator using computation expression syntax.
let argGen = ArgGenBuilder()

/// Executes a (property based) test using computation expression syntax.
let testing = TestingBuilder()
