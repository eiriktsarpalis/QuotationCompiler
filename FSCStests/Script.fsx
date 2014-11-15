#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open QuotationTransformer
open TestUtils

let ast = quotationToParsedInput <@ fun (x : float) -> round (x * x + 1.) @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation () 1.3

let tree = Ast.ofSourceString """
module Foo

let f () = fun x -> round 3.14159
"""