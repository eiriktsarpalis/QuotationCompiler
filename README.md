## QuotationCompiler

A library for converting F# quotations to untyped ASTs for the F# compiler service. 
Allows fully optimized compilation for quotations.

### Example

```fsharp
#r "QuotationCompiler.dll"

open QuotationCompiler

let fib =
    QuotationCompiler.ToFunc 
        <@
            let rec fib n =
                if n <= 1 then n
                else
                    fib (n-2) + fib(n-1)
            fib
        @>

fib () 10
```

### Build Status

Head (branch `master`), Build & Unit tests

* Windows/.NET [![Build status](https://ci.appveyor.com/api/projects/status/3a84u9wrf9xt0aks/branch/master?svg=true)](https://ci.appveyor.com/project/nessos/quotationscompiler/branch/master)
* Mac OS X/Mono 3.10 [![Build Status](https://travis-ci.org/eiriktsarpalis/QuotationsCompiler.png?branch=master)](https://travis-ci.org/eiriktsarpalis/QuotationsCompiler/branches)
