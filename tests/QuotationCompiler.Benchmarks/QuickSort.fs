module QuotationCompiler.Benchmarks.QuickSort

open FSharp.Quotations
open BenchmarkDotNet.Attributes

// Quicksort implementation taken from https://theburningmonk.com/2013/04/sorting-algorithms-in-f/
[<ReflectedDefinition>]
let quickSort (arr : int []) =
    let swap i j (arr : int []) =
        let tmp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- tmp

    let partition(arr : int [], left, right, pivotIdx) = 
        let pivot = arr.[pivotIdx]    
        swap pivotIdx right arr // move pivot to the end
    
        let mutable storeIdx = left
        for i = left to right - 1 do // left <= i < right
            if arr.[i] <= pivot then 
                swap i storeIdx arr
                storeIdx <- storeIdx + 1
        swap storeIdx right arr // move pivot to its final place
        storeIdx

    let rec loop (arr : int [], left, right) =
        // if the array has 2 or more items
        if left < right then
            // use the middle element, and sort the elements according to the value of the pivot
            // and return the new idx of the pivot
            let pivotIdx = (left + right) / 2
            let pivotNewIdx = partition(arr, left, right, pivotIdx)
            
            // recursively sort elements either side of the new pivot
            loop(arr, left, pivotNewIdx - 1)
            loop(arr, pivotNewIdx + 1, right)

    loop(arr, 0, arr.Length - 1)
    arr

let compiledMethods = Expr.GetReflectedDefinition <@ quickSort @> |> compileAll

[<MemoryDiagnoser>]
type QuickSortBenchmark() =
    let buffer = [|1 .. 100|]
    
    [<Benchmark(Description = "Managed", Baseline = true)>]
    member __.Managed() = quickSort buffer |> ignore
    [<Benchmark(Description = "Unquote")>]
    member __.Unquote() = compiledMethods.unquote.Value buffer |> ignore
    [<Benchmark(Description = "FSharp.Quotations.Evaluator")>]
    member __.Powerpack() = compiledMethods.powerpack.Value buffer |> ignore
    [<Benchmark(Description = "QuotationCompiler")>]
    member __.QCompiler() = compiledMethods.qcompiler.Value buffer |> ignore