namespace QuotationCompiler.Tests

type TestUnion =
    | A
    | B of num:int * text:string
    | C of string * int * float

type TestGenericUnion<'T> =
    | GA of 'T
    | GB of num:int * text:string

type TestRecord =
    {
        Num : int
        Text : string
        Value : float
    }

type TestGenericRecord<'T> =
    {
        GNum : int
        GText : string
        GValue : 'T
    }

exception FSharpException1
exception FSharpException2 of string

type TestClass<'T>(value : 'T) =
    let mutable value = value
    member __.Value
        with get () = value
        and set v = value <- v

    member __.TestMethod (x : int) = (value, x + 1)
    static member GenericMethod<'S> (t : 'T, s : 'S) = new TestClass<'T * 'S>(t, s)

type FSharpDelegate = delegate of int * string -> string

type DummyDisposable () =
    let mutable disposed = false
    member __.IsDisposed = disposed
    interface System.IDisposable with
        member __.Dispose () = disposed <- true

[<Struct>]
type TestStruct(age : int, name : string) =
   member __.Age = age
   member __.Name = name


type ClassWithOptionalParams(?name : string, ?age : int) =
    member __.Name = defaultArg name ""
    member __.Age = defaultArg age 0

    static member Create(?name, ?age) = new ClassWithOptionalParams(?name = name, ?age = age)


[<AutoOpen>]
module AutoOpenedModule =
    let autoOpenedValue = 42

    let genericValue<'T> = typeof<'T>

    type ClassWithOptionalParams with
        member __.ExtensionMethod (x : int) = x
        static member StaticExtensionMethod (x : int) = x
