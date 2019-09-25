#I "bin/Release/net461"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"
#r "QuotationCompiler.Tests.dll"

open FSharp.Quotations
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

#time "on"

// native
// Execute time : Real: 00:00:00.053, CPU: 00:00:00.109, GC gen0: 0, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    sqrt 2. |> ignore

// unquote
// Compile time : Real: 00:00:00.055, CPU: 00:00:00.062, GC gen0: 0, gen1: 0, gen2: 0
// Execute time : Real: 00:01:46.675, CPU: 00:01:46.673, GC gen0: 9598, gen1: 12, gen2: 1
let uqSqrt = unquote.Eval sqrtExpr
for i = 1 to 1000000 do
    uqSqrt 2. |> ignore

// quotations evaluator
// Compile time (cold) : Real: 00:00:00.405, CPU: 00:00:00.390, GC gen0: 0, gen1: 0, gen2: 0
// Compile time (warm) : Real: 00:00:00.004, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
// Execute time : Real: 00:00:00.087, CPU: 00:00:00.093, GC gen0: 15, gen1: 0, gen2: 0
let qeSqrt = powerpack.Eval sqrtExpr
for i = 1 to 1000000 do
    qeSqrt 2. |> ignore

// quotations compiler
// Compile time (cold) : Real: 00:00:05.068, CPU: 00:00:05.101, GC gen0: 11, gen1: 5, gen2: 1
// Compile time (warm) : Real: 00:00:00.149, CPU: 00:00:00.187, GC gen0: 7, gen1: 2, gen2: 0
// Execute time : Real: 00:00:00.053, CPU: 00:00:00.046, GC gen0: 0, gen1: 0, gen2: 0
let qcSqrt = compiler.Eval sqrtExpr
for i = 1 to 1000000 do
    qcSqrt 2. |> ignore