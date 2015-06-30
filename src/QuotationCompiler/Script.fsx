#I "../../bin/"
#r "QuotationCompiler.dll"

open QuotationCompiler

let fib =
    QuotationCompiler.Eval
        <@
            QuotationCompiler.Eval
                <@
                    let rec fib n =
                        if n <= 1 then n
                        else
                            fib (n-2) + fib(n-1)
                    fib
                @>
        @>

fib 10