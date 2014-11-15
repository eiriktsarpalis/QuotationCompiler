#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open quotationTransformer
open QuotationTests

let ast = quotationToParsedInput <@ fun x -> x + 41 @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]

#r "/Users/eirik/Desktop/testAssembly.dll"

Test.compiledQuotation () 1

//let tree = Ast.ofSourceString """
//module Foo
//
//let f () = fun x -> Operators.(+) (1,2)
//"""
//
//let tree' = Ast.ofSourceString ""