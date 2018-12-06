namespace QuotationCompiler.Utilities

open System
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open QuotationCompiler

/// Quotation Syntactic Equality comparer implementation
/// NB does note account for α-equivalence
type ExprEqualityComparer () =
    static let mutable exprConstInfoReader : MethodInfo option = None
    static let getExprConstInfo (o:obj) =
        match exprConstInfoReader with
        | Some m -> m.Invoke(o, [||])
        | None ->
            // ExprConstInfo is the first element of the boxed tuple (ExprConstInfo * Expr list)
            // that is returned by the ShapeCombination active pattern
            let p,_ = FSharpValue.PreComputeTuplePropertyInfo(o.GetType(), 0)
            let m = p.GetGetMethod(true)
            exprConstInfoReader <- Some m
            m.Invoke(o, [||])

    static let rec areEqualExprs (e : Expr) (e' : Expr) =
        let areEqualVars (v : Var) (v' : Var) = v.Name = v'.Name && v.Type = v'.Type

        match e, e' with
        | Value(o,_), Value(o',_) -> o = o'
        | ShapeVar v, ShapeVar v' -> areEqualVars v v'
        | ShapeLambda(v,body), ShapeLambda(v',body') -> areEqualVars v v' && areEqualExprs body body'
        | ShapeCombination(c,exprs), ShapeCombination(c', exprs') ->
            let eci = getExprConstInfo c
            let eci' = getExprConstInfo c'
            eci = eci'
                && exprs.Length = exprs'.Length
                && List.forall2 areEqualExprs exprs exprs'

        | _ -> false

    static let rec getExprHashCode (e : Expr) =
        let inline getVarHashCode (v : Var) = hash2 v.Name v.Type

        match e with
        | Value(o,_) -> hash2 0 o
        | ShapeVar v -> hash2 1 (getVarHashCode v)
        | ShapeLambda(v,body) -> hash3 2 (getVarHashCode v) (getExprHashCode body)
        | ShapeCombination(c,exprs) ->
            let eci = getExprConstInfo c
            let hashes = exprs |> List.fold (fun acc e -> hash2 acc (getExprHashCode e)) 0
            hash3 3 eci hashes

    interface IEqualityComparer<Expr> with
        member __.Equals(e1,e2) = e1.Type = e2.Type && areEqualExprs e1 e2
        member __.GetHashCode(e) = getExprHashCode e