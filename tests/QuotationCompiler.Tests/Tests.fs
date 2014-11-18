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

[<Test>]
let ``2. Simple if-then-else`` () =
    compileRun <@ if 101 % 2 = 1 then 25 + 17 else -1 @> |> should equal 42

[<Test>]
let ``2. Simple while loop`` () =
    compileRun 
        <@
            let mutable x = 0
            while x < 10 do x <- x + 1
            x
        @>
    |> should equal 10

[<Test>]
let ``2. Simple numeric for loop`` () =
    compileRun 
        <@
            let x = ref 0
            for i = 1 to 10 do
                incr x
            !x
        @>
    |> should equal 10

[<Test>]
let ``2. Simple enumerating for loop`` () =
    compileRun 
        <@
            let x = ref 0
            for i in [1 .. 100] do
                incr x
            !x
        @>
    |> should equal 100

[<Test>]
let ``2. Simple try/with`` () =
    compileRun
        <@
            try 1 / 0
            with
            | :? System.DivideByZeroException -> -1
            | e -> -20
        @>
    |> should equal -1

[<Test>]
let ``2. Simple try/finally`` () =
    let tester =
        compileRun
            <@
                let test fail =
                    let mutable isFinalized = false
                    try
                        try if fail then failwith "kaboom"
                        finally
                            isFinalized <- true
                    with _ -> ()

                    isFinalized

                test
            @>

    tester true  |> should equal true
    tester false |> should equal true

[<Test>]
let ``3. Tuple pattern match`` () =
    compileRun <@ match (1,"1") with 1,"2" -> 1 | (1,"1") -> 2 | _ -> 3 @> |> should equal 2
    compileRun <@ let (x,_) = (1,"") in x @> |> should equal 1
    compileRun <@ match "lorem ipsum", 42, "test" with _,_,"test1" -> true | _ -> false @> |> should equal false

[<Test>]
let ``3. List pattern match`` () =
    compileRun <@ match [42] with [] -> -1 | i :: [] -> i | _ -> -1 @> |> should equal 42
    compileRun <@ match [41] with [] -> -1 | [3] -> 3 | [x] -> x + 1 | _ -> -1 @> |> should equal 42
    compileRun <@ match [1;2;3] with [x;y;z] -> x + y + z | _ -> -1 @> |> should equal 6
    compileRun <@ match [Some 2; None; Some 40] with [Some i; None; Some j] -> i + j | _ -> -1 @> |> should equal 42

[<Test>]
let ``3. Record pattern match`` () =
    compileRun <@ match ref 42 with { contents = x } -> x @> |> should equal 42

[<Test>]
let ``3. Union pattern match`` () =
    compileRun <@ match None with Some 42 -> 1 | Some x -> x - 1 | None -> -1 @> |> should equal -1
    compileRun <@ match Some (1,"test") with Some(2,"test") -> false | Some(1,"test") -> true | _ -> false @> |> should equal true
    compileRun <@ match Some (Some(Some (42))) with None -> false | Some None -> false | Some (Some None) -> false | Some (Some (Some _)) -> true @> |> should equal true
    compileRun <@ match Choice<int,int>.Choice1Of2 12 with Choice2Of2 _ -> 0 | Choice1Of2 i -> i @> |> should equal 12


[<Test>]
let ``4. Sequence builders`` () =
    compileRun 
        <@ 
            seq { 
                for i in 1 .. 100 do
                    if i % 2 = 0 then
                        yield i
            }
        @> 
    |> Seq.length |> should equal 50

[<Test>]
let ``4. List builders`` () =
    compileRun 
        <@ 
            [
                for i in 1 .. 100 do
                    if i % 2 = 0 then
                        yield i
            ]
        @> 
    |> List.length |> should equal 50

[<Test>]
let ``4. Async workflows`` () =
    let fibAsync =
        compileRun 
            <@ 
                let rec fibAsync n = async {
                    if n <= 1 then return n
                    else
                        let! fnn = fibAsync(n-2)
                        let! fn = fibAsync(n-1)
                        return fnn + fn
                }

                fibAsync
            @> 

    fibAsync 10 |> Async.RunSynchronously |> should equal 55