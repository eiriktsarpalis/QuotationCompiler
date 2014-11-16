#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
            let x = ref 32

            x.contents <- 12
            x.Value
        @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation ()

let tree = Ast.ofSourceString """
module Foo

let f () =
    let x = ref (ref 42)
    (!x).current <- 12
"""
type Foo() =
    member __.Item(i,j) = i + j

let f = new Foo()

<@ f.[1,10] @>