module Testexp.Implementations.Operators
open Testexp

let inline createTest (_: ^C) (f: ^Func) (args: ^Args) =
    ((^C or ^Func) : (static member CreateTest : ^Func * ^Args -> ITesting< ^Result >) f, args)
