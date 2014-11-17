#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let f =
    QuotationCompiler.ToFunc
        <@
            let mutable x = 0
            while x < 10 do
                do System.Threading.Thread.Sleep 100
                System.Console.WriteLine "test"
                x <- x + 1
        @>

f ()


Ast.ofSourceString """
module Foo
    
let x = { V = 12 } : Foo.Bar
"""

module Foo =
    type Bar = { V : int }

let x = { V = 12 } : Foo.Bar