#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

module Ast =
    let private checker = FSharpChecker.Create()
    let ofSourceString (source : string) = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let! options = checker.GetProjectOptionsFromScript(fileName, "")
            let! parsed = checker.ParseFileInProject(fileName, source, options)
            return parsed.ParseTree
        })


let tree = Ast.ofSourceString """
module Foo.Bar

let add(x,y) = x + y

let x = 2

add(1,x + 1)
"""

let tree' = Ast.ofSourceString ""


<@ 2 @>