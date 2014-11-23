#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"
#r "QuotationCompiler.Tests.dll"

open Microsoft.FSharp.Quotations
open QuotationCompiler.Tests.Perf

// square root approximator

[<ReflectedDefinition>]
let sqrt (x : float) =
    let epsilon = 1e-10
    let rec approximate (y : float) =
        let y' = (y + x / y) / 2.
        if abs (y - y') < epsilon then y'
        else
            approximate y'

    approximate x

let sqrtExpr = Expr.GetReflectedDefinition <@ sqrt @>

#time

// native
// Real: 00:00:00.040, CPU: 00:00:00.078, GC gen0: 0, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    sqrt 2. |> ignore

// unquote
let uqSqrt = unquote.Eval sqrtExpr

// Real: 00:03:15.438, CPU: 00:03:02.203, GC gen0: 19197, gen1: 21, gen2: 2
for i = 1 to 1000000 do
    uqSqrt 2. |> ignore

// quotations evaluator
let qeSqrt = powerpack.Eval sqrtExpr

// Real: 00:00:04.304, CPU: 00:00:04.171, GC gen0: 180, gen1: 1, gen2: 0
for i = 1 to 1000000 do
    qeSqrt 2. |> ignore

// quotations compiler
let qqSqrt = compiler.Eval sqrtExpr

// Real: 00:00:00.037, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    qqSqrt 2. |> ignore