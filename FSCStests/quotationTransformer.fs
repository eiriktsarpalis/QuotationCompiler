module quotationTransformer

open System
open System.Reflection

open Microsoft.FSharp.Reflection

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

let rec sysTypeToSynType (t : System.Type) : SynType =
    if FSharpType.IsTuple t then
        let telems = 
            FSharpType.GetTupleElements t 
            |> Array.toList
            |> List.map(fun et -> false, sysTypeToSynType t)

        SynType.Tuple(telems, range0)
    elif FSharpType.IsFunction t then
        let dom, cod = FSharpType.GetFunctionElements t
        let synDom, synCod = sysTypeToSynType dom, sysTypeToSynType cod
        SynType.Fun(synDom, synCod, range0)
    elif t.IsGenericType then
        let synDef = t.GetGenericTypeDefinition() |> sysTypeToSynType
        let synParams = t.GetGenericArguments() |> Seq.map sysTypeToSynType |> Seq.toList
        SynType.App(synDef, None, synParams, [], None, (* isPostFix *) false, range0)
    elif t.IsArray then
        let synElem = t.GetElementType() |> sysTypeToSynType
        let rk = t.GetArrayRank()
        SynType.Array(rk, synElem, range0)
    else
        let longIdent = 
            t.FullName.Split('`').[0].Split([|'.';'+'|]) 
            |> Seq.map(fun n -> new Ident(n, range0))
            |> Seq.toList

        let liwd = LongIdentWithDots(longIdent, [range0])
        SynType.LongIdent liwd
        

let rec exprToAst (expr : Expr) : SynExpr =
    match expr with
    | Value(:? int as i, t) when t = typeof<int> -> SynExpr.Const(SynConst.Int32 i, range0)
    | Value(:? string as s, t) when t = typeof<string> -> SynExpr.Const(SynConst.String(s, range0), range0)
    | Value _ -> notImpl
    | Var v ->
        let ident = new Ident(v.Name, range0)
        SynExpr.Ident ident
        
    | Lambda(v, body) ->
        let vType = sysTypeToSynType v.Type
        let spat = SynSimplePat.Id(new Ident(v.Name, range0), None, false ,false ,false, range0)
        let untypedPat = SynSimplePats.SimplePats([spat], range0)
        let typedPat = SynSimplePats.Typed(untypedPat, vType, range0)
        let bodyAst = exprToAst body
        SynExpr.Lambda(false, false, typedPat, bodyAst, range0)

    | Call(None, methodInfo, args) ->
        let synArgs = List.map exprToAst args
        // TODO : type app for generic methods
        let parenArgs = SynExpr.Paren(SynExpr.Tuple(synArgs, [], range0), range0, None, range0)
        let synMethod = new Ident(methodInfo.Name, range0)
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident synMethod, parenArgs, range0)

    | _ -> notImpl


let synExprToLetBinding (expr : SynExpr) =
    let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
    let synPat = SynPat.LongIdent(LongIdentWithDots([new Ident("compiledQuotation", range0)], []), None, None, SynConstructorArgs.Pats [ SynPat.Paren(SynPat.Const(SynConst.Unit, range0), range0)], None, range0)
    let binding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, synPat, None, expr, range0, SequencePointInfoForBinding.SequencePointAtBinding range0)
    SynModuleDecl.Let(false, [binding], range0)


let letBindingToParsedInput (decl : SynModuleDecl) =
    let modl = SynModuleOrNamespace([new Ident("Test", range0)], true, [decl], PreXmlDoc.Empty,[], None, range0)
    let file = ParsedImplFileInput("/test.fs", false, QualifiedNameOfFile(new Ident("Test", range0)), [],[], [modl],false)
    ParsedInput.ImplFile file


let quotationToParsedInput : Expr -> ParsedInput = exprToAst >> synExprToLetBinding >> letBindingToParsedInput