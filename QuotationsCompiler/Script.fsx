#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationsCompiler

let ast : ParsedInput = 
    Transformer.quotationToParsedInput 
        <@ 
            let protect (f : int -> int) x = try f x |> Choice1Of2 with e -> Choice2Of2 e

            protect (fun x -> 1 / x) 0
        @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation ()

let tree = Ast.ofSourceString """
module Foo

let f () = Some 42
"""