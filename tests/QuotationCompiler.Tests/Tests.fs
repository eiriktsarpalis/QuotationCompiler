module QuotationCompiler.Tests

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Quotations

let compile (e : Expr<'T>) = QuotationCompiler.ToFunc e


[<Test>]
let ``1. Constant leaf`` () =
    let testLeaf value = 
        let f = compile <@ value @>
        f () |> should equal value

    testLeaf true
    testLeaf 78uy
    testLeaf 99y
    testLeaf 'c'
    testLeaf 1.1231M
    testLeaf 1970s
    testLeaf 1970
    testLeaf 1231231L
    testLeaf 1970us
    testLeaf 1970u
    testLeaf 1231231uL
    testLeaf 1231n
    testLeaf 1231un
    testLeaf 3.1415926
    testLeaf "lorem ipsum"
    testLeaf ()
    testLeaf [|1uy..100uy|]
    testLeaf [|1us..100us|]