namespace Testexp
open System
open System.Text
open System.Runtime.CompilerServices

/// Represents testing contexts.
type TestContext<'T> =
    {
        /// Gets arguments passed to the target function.
        Arguments: ITuple
        /// Gets return value of the target function.
        Result: Result<'T, exn>
    }

    override this.ToString() =
        let builder = StringBuilder()
        for i in 0..this.Arguments.Length - 1 do
            builder
                .Append("Argument")
                .Append(i)
                .Append(": ")
                .AppendLine($"%A{this.Arguments[i]}")
            |> ignore
        match this.Result with
        | Ok(result) -> builder.Append("Return: ").AppendLine($"%A{result}")
        | Error(exn) -> builder.Append("Exception: ").AppendLine($"%O{exn}")
        |> ignore
        builder.ToString()


/// Represents untyped assertion functions.
[<Struct>]
type Assertion = internal Assertion of assertion: (Result<obj, exn> -> TestContext<obj> -> unit)


/// Represents assertable testing functions.
type ITesting<'T> =
    abstract member Test : assertion: Assertion -> unit
    abstract member Test : assertion: ('T -> TestContext<'T> -> unit) -> unit
    abstract member Test : assertions: ('T -> TestContext<'T> -> unit) list -> unit


/// Thrown when tests fail.
[<AbstractClass>]
type ValidationExceptionBase(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    abstract member Arguments: ITuple
    abstract member Result: Result<obj, exn>


/// Thrown when tests fail.
type ValidationException<'T>(message: string, testContext: TestContext<'T>, innerException: exn) =
    inherit ValidationExceptionBase(message + "\r\n\r\n" + string testContext, innerException)

    new(message, testContext) = ValidationException<'T>(message, testContext, null)

    member _.TestContext = testContext

    override _.Arguments = testContext.Arguments

    override _.Result = testContext.Result |> Result.map box


/// For internal used.
[<Struct; NoComparison; StructuralEquality>]
type RandomState = internal RandomState of uint64 with
    static member Default = RandomState(0uL)


/// Represents argument generators for property based testing.
type IArgumentGenerator<'T> =
    abstract member Generate : state: byref<RandomState> -> 'T
