#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

#r "/Users/eirik/Desktop/test.dll"

let f =
    QuotationCompiler.ToFunc 
        <@
            Async.RunSynchronously(async { return 42})
        @>

f ()

let ast =
    <@ 
        Async.RunSynchronously(async { return 42})
    @>
    |> QuotationCompiler.ToParsedInput
    |> snd

let tree = Ast.ofSourceString """
module Foo
    
let x = Test.Foo(?name = None)
"""