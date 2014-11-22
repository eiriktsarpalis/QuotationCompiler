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