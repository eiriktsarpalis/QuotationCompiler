## QuotationCompiler

A small library for compiling code quotations using the F# compiler service.
Its primary functionality is transforming quotation trees to untyped ASTs used by the F# compiler.
Since code is generated using the F# compiler proper, the end result is fully efficient and optimized.

Package available on [NuGet](https://www.nuget.org/packages/QuotationCompiler/).

### Example

```fsharp
#r "QuotationCompiler.dll"

open QuotationCompiler

let hello : unit -> unit = QuotationCompiler.ToFunc <@ printfn "Hello, world!" @>

hello ()
```

### Performance

As can be expected, performance exceeds all other quotation evaluation libraries.
Here is a [benchmark](https://github.com/eiriktsarpalis/QuotationsCompiler/blob/master/tests/QuotationCompiler.Tests/perf.fsx) for a tail recursive algorithm:
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

| Library                                                                                 | Compilation time (cold) | Compilation time (warm) |
|-----------------------------------------------------------------------------------------|-------------------------|-------------------------|
| Native                                                                                  | N/A                     | N/A                     |
| [Unquote](https://code.google.com/p/unquote/)                                           | 00:00:00.055            | 00:00:00.000            |
| [FSharp.Quotations.Evaluator](http://fsprojects.github.io/FSharp.Quotations.Evaluator/) | 00:00:00.405            | 00:00:00.003            |
| QuotationCompiler                                                                       | 00:00:05.068            | 00:00:00.161            |

Executing the compiled functions 10^6 times produced the following results:

| Library                                                                                 | Execution time | GC gen0,1,2 |
|-----------------------------------------------------------------------------------------|----------------|-------------|
| Native                                                                                  | 00:00:00.053   | 0,0,0       |
| [Unquote](https://code.google.com/p/unquote/)                                           | 00:01:46.675   | 9598,12,1   |
| [FSharp.Quotations.Evaluator](http://fsprojects.github.io/FSharp.Quotations.Evaluator/) | 00:00:00.087   | 15,0,0      |
| QuotationCompiler                                                                       | 00:00:00.053   | 0,0,0       |

### Limitations

The library currently has a few limitations
* No support for dynamic assemblies. Quotations inherently belong to the runtime and ASTs are inherently static. It is impossible to compile quotations that reference types/code defined in F# interactive.
* F# metadata issues. Things may break when referencing [generic values](https://visualfsharp.codeplex.com/workitem/178), extension methods or [CompiledName](https://visualfsharp.codeplex.com/workitem/177) types.

### Build Status

Head (branch `master`), Build & Unit tests

* Windows [![Build status](https://ci.appveyor.com/api/projects/status/79arr40vmvtt5tb9/branch/master?svg=true)](https://ci.appveyor.com/project/nessos/quotationcompiler/branch/master)
* Linux [![Build Status](https://travis-ci.org/eiriktsarpalis/QuotationCompiler.png?branch=master)](https://travis-ci.org/eiriktsarpalis/QuotationCompiler/branches)