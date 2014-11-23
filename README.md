## QuotationCompiler

A small library for compiling code quotations using the F# compiler service.
Its primary functionality is transforming quotation trees to untyped ASTs used by the F# compiler.
Since code is generated using the F# compiler proper, the end result is fully efficient and optimized.

### Example

```fsharp
#r "QuotationCompiler.dll"

open QuotationCompiler

let hello : unit -> unit = QuotationCompiler.ToFunc <@ printfn "Hello, world!" @>

hello ()
```

### Performance

As can be expected, performance greatly exceeds all other quotation evaluation libraries.
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
| Library                     | Performance                                                              |
|-----------------------------|--------------------------------------------------------------------------|
| Native                      | Real: 00:00:00.040, CPU: 00:00:00.078, GC gen0: 0, gen1: 0, gen2: 0      |
| Unquote                     | Real: 00:03:15.438, CPU: 00:03:02.203, GC gen0: 19197, gen1: 21, gen2: 2 |
| FSharp.Quotations.Evaluator | Real: 00:00:04.304, CPU: 00:00:04.171, GC gen0: 180, gen1: 1, gen2: 0    |
| QuotationsCompiler          | Real: 00:00:00.037, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0      |

### Limitations

The library currently has a few limitations
* No support for dynamic assemblies. Quotations inherently belong to the runtime and ASTs are inherently static. It is impossible to compile quotations that reference types/code defined in F# interactive.
* F# metadata issues. Thing may break when referencing [generic values](https://visualfsharp.codeplex.com/workitem/178) or [CompiledName](https://visualfsharp.codeplex.com/workitem/177) types.

### Build Status

Head (branch `master`), Build & Unit tests

* Windows/.NET [![Build status](https://ci.appveyor.com/api/projects/status/3a84u9wrf9xt0aks/branch/master?svg=true)](https://ci.appveyor.com/project/nessos/quotationscompiler/branch/master)
* Mac OS X/Mono 3.10 [![Build Status](https://travis-ci.org/eiriktsarpalis/QuotationsCompiler.png?branch=master)](https://travis-ci.org/eiriktsarpalis/QuotationsCompiler/branches)
