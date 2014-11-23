#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let fib =
    QuotationCompiler.ToFunc 
        <@
            let rec fib n =
                if n <= 1 then n
                else
                    fib (n-2) + fib(n-1)
            fib
        @>

fib () 10

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