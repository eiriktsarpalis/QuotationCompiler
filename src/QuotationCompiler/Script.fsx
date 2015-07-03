#I "../../bin/"
#r "QuotationCompiler.dll"

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open QuotationCompiler

// 1. hello world
let hello = QuotationCompiler.ToFunc <@ printfn "Hello, World!" @>

hello ()

// 2. fibonacci
let fib =
    QuotationCompiler.Eval
        <@
            let rec fib n =
                if n <= 1 then n
                else
                    fib (n-2) + fib(n-1)
            fib
        @>

fib 10

QuotationCompiler.Eval <@ [||] : int [] @>
QuotationCompiler.Eval <@ [|1;2;3|] @>