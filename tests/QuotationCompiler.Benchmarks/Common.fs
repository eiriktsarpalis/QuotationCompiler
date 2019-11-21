[<AutoOpen>]
module QuotationCompiler.Benchmarks.Common

open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.Patterns
open QuotationCompiler

type Expr with
    static member GetReflectedDefinition (e : Expr<'T>) =
        let rec methodOf (e : Expr) =
            match e with
            | Lambda(_,b) -> methodOf b
            | Call(_,m,_) -> m :> MethodBase
            | NewObject(c,_) -> c :> MethodBase
            | PropertyGet(_,p,_) -> p.GetGetMethod(true) :> MethodBase
            | _ -> invalidArg "e" "should be method call."

        let m = methodOf e 
        let re = Expr.TryGetReflectedDefinition m |> Option.get
        Expr.Cast<'T>(re)

let compileAll (e : Expr<'T>) =
    {|
        powerpack = lazy(FSharp.Quotations.Evaluator.QuotationEvaluator.Evaluate e)
        unquote = lazy(Swensen.Unquote.Operators.eval e)
        qcompiler = lazy(QuotationCompiler.Eval e |> Async.RunSynchronously)
    |}