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