module quotationTransformer

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let inline notImpl<'T> : 'T = raise <| new NotImplementedException()

type TransformerState =
    {
        Dependencies : Map<string, Assembly>
    }
with
    static member Empty = { Dependencies = Map.empty }

let updateDependencies (state : TransformerState) (t : Type) =
    let key = t.Assembly.FullName
    match state.Dependencies.TryFind key with
    | Some _ -> state
    | None -> { state with Dependencies = Map.add key t.Assembly state.Dependencies }

let sysTypeToSynType (t : System.Type) = notImpl<SynType>

let rec exprToAst (expr : Expr) : SynExpr =
    match expr with
    | Value(:? int as i, t) when t = typeof<int> -> SynExpr.Const(SynConst.Int32 i, range0)
    | Value(:? string as s, t) when t = typeof<string> -> SynExpr.Const(SynConst.String(s, range0), range0)
    | Value _ -> notImpl
    | Lambda(v, body) ->
        let vType = sysTypeToSynType v.Type
        let spat = SynSimplePat.Id(new Ident(v.Name, range0), None, false ,false ,false, range0)
        let untypedPat = SynSimplePats.SimplePats([spat], range0)
        let typedPat = SynSimplePats.Typed(untypedPat, vType, range0)
        let bodyAst = exprToAst expr
        SynExpr.Lambda(false, false, typedPat, bodyAst, range0)

    | Call(None, methodInfo, args) ->
        let synArgs = List.map exprToAst args
        // TODO : type app for generic methods
        let parenArgs = SynExpr.Paren(SynExpr.Tuple(synArgs, [], range0), range0, None, range0)
        let synMethod = new Ident(methodInfo.Name, range0)
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident synMethod, parenArgs, range0)

    | _ -> notImpl