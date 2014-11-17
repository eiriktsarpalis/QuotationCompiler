module QuotationCompiler.Tests.TestModule

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Quotations

open QuotationCompiler

let compile (e : Expr<'T>) = QuotationCompiler.ToFunc e
let compileRun (e : Expr<'T>) = QuotationCompiler.ToFunc e ()


[<Test>]
let ``1. Constant leaf`` () =
    let testLeaf value = compileRun <@ value @> |> should equal value

    testLeaf true
    testLeaf 78uy
    testLeaf 99y
    testLeaf 'c'
    testLeaf 1.1231M
    testLeaf 1970s
    testLeaf 1970
    testLeaf 1231231L
    testLeaf 1970us
    testLeaf 1970u
    testLeaf 1231231uL
    testLeaf 1231n
    testLeaf 1231un
    testLeaf 3.1415926
    testLeaf "lorem ipsum"
    testLeaf ()
    testLeaf [|1uy..100uy|]
    testLeaf [|1us..100us|]

[<Test>]
let ``1. List leaf`` () =
    compileRun <@ [] : int list @> |> should equal List.empty<int>
    compileRun <@ [1;2;3] @> |> should equal [1;2;3]
    compileRun <@ 1 :: 2 :: 3 :: [] @> |> should equal [1;2;3]
    compileRun <@ [1 .. 10] @> |> should equal [1..10]

[<Test>]
let ``1. Array leaf`` () =
    compileRun <@ [||] : int [] @> |> should equal Array.empty<int>
    compileRun <@ [|1;2;3|] @> |> should equal [|1;2;3|]
    compileRun <@ [|1 .. 10|] @> |> should equal [|1..10|]

[<Test>]
let ``1. Union leaf`` () =
    compileRun <@ None : int option @> |> should equal Option<int>.None
    compileRun <@ Some (1,"lorem ipsum") @> |> should equal (Some (1,"lorem ipsum"))
    compileRun <@ Choice1Of2 "value" : Choice<string,exn> @> |> should equal (Choice<string,exn>.Choice1Of2 "value")
    compileRun <@ Choice1Of2 "value" : Choice<string,exn> @> |> should equal (Choice<string,exn>.Choice1Of2 "value")
//    compileRun <@ A @> |> should equal A
//    compileRun <@ B(42, "lorem ipsum") @> |> should equal (B(42, "lorem ipsum"))
//    compileRun <@ GA (1,1) @> |> should equal (GA(1,1))

[<Test>]
let ``1. Record leaf`` () =
    compileRun <@ { contents = 42 } @> |> should equal (ref 42)
//    compileRun <@ { Num = 42 ; Text = "text" ; Value = 3. } @> |> should equal { Num = 42 ; Text = "text" ; Value = 3. }
//    compileRun <@ { GNum = 42 ; GText = "text" ; GValue = Some 3. } @> |> should equal { GNum = 42 ; GText = "text" ; GValue = Some 3. }

[<Test>]
let ``1. Tuple leaf`` () =
    compileRun <@ (1,"2") @> |> should equal (1,"2")
    compileRun <@ (1,"2", Some 42, [1..2]) @> |> should equal (1,"2", Some 42, [1..2])


[<Test>]
let ``2. Simple let binding`` () =
    compileRun <@ let x = 1 + 1 in x + x @> |> should equal 4

[<Test>]
let ``2. Nested let binding`` () =
    compileRun 
        <@ 
            let x = let z = 1 in z + z
            let y = let z = 1. in z + z
            (x,y) 
        @> |> should equal (2,2.)

[<Test>]
let ``2. Recursive binding`` () =
    compileRun 
        <@ 
            let rec fib n =
                if n <= 1 then n
                else
                    fib(n-2) + fib(n-1)

            fib 10
        @> |> should equal 55

[<Test>]
let ``2. Mutual recursive binding`` () =
    let even,odd =
        compileRun
            <@
                let rec even n =
                    if n = 0 then true
                    else odd (n-1)

                and odd n =
                    if n = 0 then false
                    else even (n-1)

                even,odd
            @>

    for i in 0 .. 10 do
        even i |> should equal (i % 2 = 0)
        odd i |> should equal (i % 2 = 1)

[<Test>]
let ``2. Simple Lambda`` () =
    let f = compileRun <@ fun x -> x + 1 @>
    f 0 |> should equal 1
    let g = compileRun <@ fun x y -> x * y @>
    g 2 2 |> should equal 4

[<Test>]
let ``2. Higher-order function`` () =
    let twice = compileRun <@ let twice (f : int -> int) = f << f in twice @>
    twice (fun x -> x + x) 1 |> should equal 4