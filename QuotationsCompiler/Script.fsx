#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
            match box 42 with
            | :? int as x -> x = 42
            | _ -> false
//            let mutable x = 0
//            while x < 10 do
//                System.Console.WriteLine "test"
//                x <- x + 1
        @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation ()

let tree = Ast.ofSourceString """
module Foo

let f () =
    match box 42 with
    | :? int as x -> x = 42
    | _ -> false
"""