#I "bin/Debug/net461/"
#r "QuotationCompiler.dll"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

#time "on"

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