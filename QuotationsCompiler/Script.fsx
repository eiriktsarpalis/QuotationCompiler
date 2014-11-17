#r "../packages/FSharp.Compiler.Service.0.0.76/lib/net45/FSharp.Compiler.Service.dll"
#r "bin/Debug/QuotationCompiler.dll"

open QuotationsCompiler

let f = 
    QuotationsCompiler.ToFunc
        <@
            let mutable x = 0
            while x < 10 do
                System.Console.WriteLine "test"
                x <- x + 1
        @>

f ()