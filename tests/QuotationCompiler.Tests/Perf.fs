module QuotationCompiler.Tests.Perf

open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

open FSharp.Quotations.Evaluator
open Swensen.Unquote
open QuotationCompiler

type IQuotationEvaluator =
    abstract Eval : Expr<'T> -> 'T

let rec methodOf (e : Expr) =
    match e with
    | Lambda(_,b) -> methodOf b
    | Call(_,m,_) -> m :> MethodBase
    | NewObject(c,_) -> c :> MethodBase
    | PropertyGet(_,p,_) -> p.GetGetMethod(true) :> MethodBase
    | _ -> invalidArg "e" "should be method call."

type Expr with
    static member GetReflectedDefinition (e : Expr<'T>) =
        let m = methodOf e 
        let re = Expr.TryGetReflectedDefinition m |> Option.get
        Expr.Cast<'T>(re)

let powerpack =
    {
        new IQuotationEvaluator with
            member __.Eval e = QuotationEvaluator.Compile e
    }

let unquote =
    {
        new IQuotationEvaluator with
            member __.Eval e = eval e
    }

let compiler =
    {
        new IQuotationEvaluator with
            member __.Eval e = QuotationCompiler.ToFunc e ()
    }