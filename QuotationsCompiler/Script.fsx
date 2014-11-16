#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
//            let x = 12
            (1).GetType()

//            let rec fib n =
//                if n <= 1 then n
//                else
//                    fib(n-1) + fib(n-2)
//
//            fib
        @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation () 10

let t = typeof<int>

<@ t.GetArrayRank() @>

let tree = Ast.ofSourceString """
module Foo

let f () = (1 + 1).Bar()
"""