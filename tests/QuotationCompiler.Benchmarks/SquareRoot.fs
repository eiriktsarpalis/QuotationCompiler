module QuotationCompiler.Benchmarks.SquareRoot

open FSharp.Quotations
open BenchmarkDotNet.Attributes

// Square root approximation algorithm taken from SICP

[<ReflectedDefinition>]
let sqrt (x : float) =
    let epsilon = 1e-10
    let rec approximate (y : float) =
        let y' = (y + x / y) / 2.
        if abs (y - y') < epsilon then y'
        else
            approximate y'

    approximate x

let compiledMethods = Expr.GetReflectedDefinition <@ sqrt @> |> compileAll

[<MemoryDiagnoser>]
type SqrtBenchmark() =
    let inputValue = 1821.
    
    [<Benchmark(Description = "Managed", Baseline = true)>]
    member __.Managed() = sqrt inputValue |> ignore
    [<Benchmark(Description = "Unquote")>]
    member __.Unquote() = compiledMethods.unquote.Value inputValue |> ignore
    [<Benchmark(Description = "FSharp.Quotations.Evaluator")>]
    member __.Powerpack() = compiledMethods.powerpack.Value inputValue |> ignore
    [<Benchmark(Description = "QuotationCompiler")>]
    member __.QCompiler() = compiledMethods.qcompiler.Value inputValue |> ignore