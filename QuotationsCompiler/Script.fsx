#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
            try
                try 1 / 0 |> ignore ; 0 with e -> e.GetHashCode()
            finally
                System.Console.WriteLine "complete"
//            let protect (f : int -> int) x = try f x |> Choice1Of2 with e -> Choice2Of2 e
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

let f () = try 1 with e -> ()
"""

<@ typeof<int> @>