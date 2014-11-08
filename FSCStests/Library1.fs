module QuotationTests
    
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open quotationTransformer

module Ast =

    let ofSourceString (source : string) = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let checker = FSharpChecker.Create()
            let! options = checker.GetProjectOptionsFromScript(fileName, "")
            let! parsed = checker.ParseFileInProject(fileName, source, options)
            return parsed.ParseTree
        })

    let compile outputDir dependencies (ast : ParsedInput list) =
        let sscs = new SimpleSourceCodeServices()
        sscs.Compile(ast, "testAssembly", outputDir + "/testAssembly.dll", dependencies)


let ast = quotationToParsedInput <@ fun x -> x + 1 @>

let err = Ast.compile "/Users/eirik/Desktop" [] [ast]
//
//
//let tree = Ast.ofSourceString """
//module Foo.Bar
//
//let f = typeof<int list>
//"""
//
//let errors = Ast.compile "/Users/eirik/Desktop" [] [tree.Value]