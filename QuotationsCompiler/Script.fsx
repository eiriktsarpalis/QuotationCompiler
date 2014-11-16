#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
            let v = if System.IO.File.Exists "1" then None else Some 12

            match v with
            | Some 2 -> 2
            | Some x -> x + 1
            | None -> 0

//
//            protect (fun x -> 1 / x) 0
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

<@ [|1..100|] @>

op_Range 1 2

let tree = Ast.ofSourceString """
module Foo

let f () = Some 42
"""

<@ Some 42 @>