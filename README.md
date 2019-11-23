## QuotationCompiler

A small library for compiling code quotations using the F# compiler service.
Its primary functionality is transforming quotation trees to untyped ASTs used by the F# compiler.
Since code is generated using the F# compiler proper, the end result is fully efficient and optimized.

Package available on [NuGet](https://www.nuget.org/packages/QuotationCompiler/).

### Example

```fsharp
#r "QuotationCompiler.dll"

open QuotationCompiler

let hello : unit -> unit = QuotationCompiler.ToFunc <@ printfn "Hello, world!" @> |> Async.RunSynchronously

hello ()
```

### Performance

As can be expected, performance exceeds all other quotation evaluation libraries.
Here is a [benchmark](https://github.com/eiriktsarpalis/QuotationCompiler/blob/114b29a1ae133c0754d93322136193a71e43699b/tests/QuotationCompiler.Benchmarks/SquareRoot.fs) for a tail recursive algorithm:
```fsharp
[<ReflectedDefinition>]
let sqrt (x : float) =
    let epsilon = 1e-10
    let rec approximate (y : float) =
        let y' = (y + x / y) / 2.
        if abs (y - y') < epsilon then y'
        else
            approximate y'

    approximate x
```

``` ini

BenchmarkDotNet=v0.12.0, OS=Windows 10.0.18363
Intel Core i7-8665U CPU 1.90GHz (Coffee Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=3.1.100-preview3-014645
  [Host]     : .NET Core 3.1.0 (CoreCLR 4.700.19.53102, CoreFX 4.700.19.55104), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 3.1.0 (CoreCLR 4.700.19.53102, CoreFX 4.700.19.55104), X64 RyuJIT


```
|                      Method |       Mean |      Error |    StdDev |     Median |  Ratio | RatioSD | Gen 0 | Gen 1 | Gen 2 | Allocated |
|---------------------------- |-----------:|-----------:|----------:|-----------:|-------:|--------:|------:|------:|------:|----------:|
|                     Managed |   2.250 us |  0.5888 us |  1.718 us |   1.300 us |   1.00 |    0.00 |     - |     - |     - |         - |
|                     Unquote | 291.363 us | 21.7297 us | 61.289 us | 285.150 us | 187.34 |  100.18 |     - |     - |     - |   74624 B |
| FSharp.Quotations.Evaluator |   9.622 us |  1.9767 us |  5.671 us |   7.100 us |   6.00 |    4.87 |     - |     - |     - |      64 B |
|           QuotationCompiler |   4.478 us |  1.5073 us |  4.325 us |   1.900 us |   2.46 |    2.78 |     - |     - |     - |         - |

A [quicksort implementation benchmark](https://github.com/eiriktsarpalis/QuotationCompiler/blob/114b29a1ae133c0754d93322136193a71e43699b/tests/QuotationCompiler.Benchmarks/QuickSort.fs):

|                      Method |          Mean |       Error |       StdDev |        Median |    Ratio |  RatioSD |     Gen 0 | Gen 1 | Gen 2 | Allocated |
|---------------------------- |--------------:|------------:|-------------:|--------------:|---------:|---------:|----------:|------:|------:|----------:|
|                     Managed |      3.862 us |   0.7029 us |     2.039 us |      2.700 us |     1.00 |     0.00 |         - |     - |     - |         - |
|                     Unquote | 10,539.083 us | 605.5924 us | 1,776.098 us | 10,532.100 us | 3,364.69 | 1,415.76 | 1000.0000 |     - |     - | 6318024 B |
| FSharp.Quotations.Evaluator |    112.261 us |   5.4346 us |    15.680 us |    105.100 us |    35.54 |    14.36 |         - |     - |     - |    6680 B |
|           QuotationCompiler |      7.259 us |   1.8754 us |     5.441 us |      4.200 us |     2.24 |     1.92 |         - |     - |     - |         - |

### Limitations

The library currently has a few limitations
* No support for dynamic assemblies. Quotations inherently belong to the runtime and ASTs are inherently static. It is impossible to compile quotations that reference types/code defined in F# interactive.
* F# metadata issues. Things may break when referencing [generic values](https://visualfsharp.codeplex.com/workitem/178), extension methods or [CompiledName](https://visualfsharp.codeplex.com/workitem/177) types.

### Build Status

Head (branch `master`), Build & Unit tests

* Windows [![Build status](https://ci.appveyor.com/api/projects/status/79arr40vmvtt5tb9/branch/master?svg=true)](https://ci.appveyor.com/project/nessos/quotationcompiler/branch/master)
* Linux [![Build Status](https://travis-ci.org/eiriktsarpalis/QuotationCompiler.svg?branch=master)](https://travis-ci.org/eiriktsarpalis/QuotationCompiler)
