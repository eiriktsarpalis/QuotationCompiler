module QuotationsCompiler.Transformer

open System
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let inline notImpl<'T> e : 'T = raise <| new NotImplementedException(sprintf "%O" e)

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

let tryParseRange (expr : Expr) =
    match expr.CustomAttributes with
    | [ NewTuple [_; NewTuple [ Value (:? string as file, _); 
                                Value (:? int as r1, _); Value (:? int as c1, _); 
                                Value (:? int as r2, _); Value(:? int as c2, _)]]] -> 
        let p1 = mkPos r1 c1
        let p2 = mkPos r2 c2
        Some <| mkRange file p1 p2
    | _ -> None


type MemberInfo with
    member m.TryGetCustomAttribute<'Attr when 'Attr :> System.Attribute> () =
        let attrs = m.GetCustomAttributes<'Attr> ()
        if Seq.isEmpty attrs then None
        else
            Some(Seq.head attrs)

    member m.ContainsAttribute<'Attr when 'Attr :> System.Attribute> () =
        m.GetCustomAttributes<'Attr> () |> Seq.isEmpty |> not

    member m.Assembly = match m with :? Type as t -> t.Assembly | _ -> m.DeclaringType.Assembly

let inline mkIdent range text = new Ident(text, range)
let inline mkLongIdent range (li : Ident list) = LongIdentWithDots(li, [range])
let inline mkVarPat range (v : Quotations.Var) = 
    let lident = mkLongIdent range [mkIdent range v.Name]
    SynPat.LongIdent(lident, None, None, SynConstructorArgs.Pats [], None, range)

let private moduleSuffixRegex = new Regex(@"^(.*)Module$", RegexOptions.Compiled)
let private fsharpPrefixRegex = new Regex(@"^FSharp(.*)`[0-9]+$", RegexOptions.Compiled)
let getFSharpName (m : MemberInfo) =
    match m.TryGetCustomAttribute<CompilationSourceNameAttribute> () with
    | Some a -> a.SourceName
    | None ->

    // this is a hack; need a better solution in the long term
    if m.Assembly = typeof<int option>.Assembly && fsharpPrefixRegex.IsMatch m.Name then
        let rm = fsharpPrefixRegex.Match m.Name
        rm.Groups.[1].Value
    else

    match m, m.TryGetCustomAttribute<CompilationRepresentationAttribute> () with
    | :? Type as t, Some attr when attr.Flags.HasFlag CompilationRepresentationFlags.ModuleSuffix && FSharpType.IsModule t ->
        let rm = moduleSuffixRegex.Match m.Name
        if rm.Success then rm.Groups.[1].Value
        else
            m.Name
    | _ -> m.Name.Split('`').[0]

let getMemberPath range (m : MemberInfo) =
    let rec aux (m : MemberInfo) = seq {
        match m.DeclaringType with
        | null -> yield! (m :?> Type).Namespace.Split('.') 
        | dt -> yield! aux dt 
        
        yield getFSharpName m
    }

    aux m |> Seq.map (mkIdent range) |> Seq.toList

let rec sysTypeToSynType (range : range) (t : System.Type) : SynType =
    if FSharpType.IsTuple t then
        let telems = 
            FSharpType.GetTupleElements t 
            |> Array.toList
            |> List.map(fun et -> false, sysTypeToSynType range et)

        SynType.Tuple(telems, range)
    elif FSharpType.IsFunction t then
        let dom, cod = FSharpType.GetFunctionElements t
        let synDom, synCod = sysTypeToSynType range dom, sysTypeToSynType range cod
        SynType.Fun(synDom, synCod, range)
    elif t.IsGenericType && not t.IsGenericTypeDefinition then
        let synDef = t.GetGenericTypeDefinition() |> sysTypeToSynType range
        let synParams = t.GetGenericArguments() |> Seq.map (sysTypeToSynType range) |> Seq.toList
        SynType.App(synDef, None, synParams, [], None, (* isPostFix *) false, range)
    elif t.IsArray then
        let synElem = t.GetElementType() |> sysTypeToSynType range
        let rk = t.GetArrayRank()
        SynType.Array(rk, synElem, range)
    else
        let liwd = LongIdentWithDots(getMemberPath range t, [range])
        SynType.LongIdent liwd

let mkUciIdent range (uci : UnionCaseInfo) =
    let path = getMemberPath range uci.DeclaringType
    LongIdentWithDots(path @ [mkIdent range uci.Name], [range])

let mkUciCtor range (uci : UnionCaseInfo) =
    if uci.DeclaringType.IsGenericType then
        let synUnion = SynExpr.LongIdent(false, mkLongIdent range (getMemberPath range uci.DeclaringType), None, range)
        let synArgs = uci.DeclaringType.GetGenericArguments() |> Seq.map (sysTypeToSynType range) |> Seq.toList
        let unionTy = SynExpr.TypeApp(synUnion, range, synArgs, [range], None, range, range)
        SynExpr.DotGet(unionTy, range, mkLongIdent range [mkIdent range uci.Name], range)
    else
        SynExpr.LongIdent(false, mkUciIdent range uci, None, range)

let tryGetCurriedFunctionGroupings (m : MethodInfo) =
    match m.TryGetCustomAttribute<CompilationArgumentCountsAttribute> () with
    | None -> None
    | Some a -> Some(a.Counts |> Seq.toList)

let sysMemberToSynMember range (m : MemberInfo) =
    let liwd = LongIdentWithDots(getMemberPath range m, [range])
    SynExpr.LongIdent(false, liwd, None, range)
        

let rec exprToAst (expr : Expr) : SynExpr =
    let range = defaultArg (tryParseRange expr) range0
    match expr with
    // parse for constants
    | Value(:? bool as b, t) when t = typeof<bool> -> SynExpr.Const(SynConst.Bool b, range)
    | Value(:? byte as b, t) when t = typeof<byte> -> SynExpr.Const(SynConst.Byte b, range)
    | Value(:? sbyte as b, t) when t = typeof<sbyte> -> SynExpr.Const(SynConst.SByte b, range)
    | Value(:? char as c, t) when t = typeof<char> -> SynExpr.Const(SynConst.Char c, range)
    | Value(:? decimal as d, t) when t = typeof<decimal> -> SynExpr.Const(SynConst.Decimal d, range)
    | Value(:? int16 as i, t) when t = typeof<int16> -> SynExpr.Const(SynConst.Int16 i, range)
    | Value(:? int32 as i, t) when t = typeof<int32> -> SynExpr.Const(SynConst.Int32 i, range)
    | Value(:? int64 as i, t) when t = typeof<int64> -> SynExpr.Const(SynConst.Int64 i, range)
    | Value(:? uint16 as i, t) when t = typeof<uint16> -> SynExpr.Const(SynConst.UInt16 i, range)
    | Value(:? uint32 as i, t) when t = typeof<uint32> -> SynExpr.Const(SynConst.UInt32 i, range)
    | Value(:? uint64 as i, t) when t = typeof<uint64> -> SynExpr.Const(SynConst.UInt64 i, range)
    | Value(:? IntPtr as i, t) when t = typeof<IntPtr> -> SynExpr.Const(SynConst.IntPtr(int64 i), range)
    | Value(:? UIntPtr as i, t) when t = typeof<UIntPtr> -> SynExpr.Const(SynConst.UIntPtr(uint64 i), range)
    | Value(:? single as f, t) when t = typeof<single> -> SynExpr.Const(SynConst.Single f, range)
    | Value(:? double as f, t) when t = typeof<double> -> SynExpr.Const(SynConst.Double f, range)
    | Value(:? string as s, t) when t = typeof<string> -> SynExpr.Const(SynConst.String(s, range), range)
    | Value(:? unit, t) when t = typeof<unit> -> SynExpr.Const(SynConst.Unit, range)
    | Value(:? (byte[]) as bs, t) when t = typeof<byte[]> -> SynExpr.Const(SynConst.Bytes(bs, range), range)
    | Value(:? (uint16[]) as is, t) when t = typeof<uint16[]> -> SynExpr.Const(SynConst.UInt16s is, range)
    | Value (_,t) -> raise <| new NotSupportedException(sprintf "Quotation captures closure of type %O." t)
    // Lambda
    | Var v ->
        let ident = mkIdent range v.Name
        SynExpr.Ident ident
        
    | Lambda(v, body) ->
        let vType = sysTypeToSynType range v.Type
        let spat = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
        let untypedPat = SynSimplePats.SimplePats([spat], range)
        let typedPat = SynSimplePats.Typed(untypedPat, vType, range)
        let bodyAst = exprToAst body
        SynExpr.Lambda(false, false, typedPat, bodyAst, range)

    | LetRecursive(bindings, body) ->
        let mkBinding (v : Var, bind : Expr) =
            let vType = sysTypeToSynType range v.Type
            let untypedPat = mkVarPat range v
            let typedPat = SynPat.Typed(untypedPat, vType, range)
            let synBind = exprToAst bind
            let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
            SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, typedPat, None, synBind, range, SequencePointInfoForBinding.SequencePointAtBinding range)

        let bindings = List.map mkBinding bindings
        let synBody = exprToAst body
        SynExpr.LetOrUse(true, false, bindings, synBody, range)

    | Let(v, bind, body) ->
        let vType = sysTypeToSynType range v.Type
        let untypedPat = mkVarPat range v
        let typedPat = SynPat.Typed(untypedPat, vType, range)
        let synBind = exprToAst bind
        let synBody = exprToAst body
        let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
        let synBinding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, typedPat, None, synBind, range, SequencePointInfoForBinding.SequencePointAtBinding range)
        SynExpr.LetOrUse(false, false, [synBinding], synBody, range)

    | Application(left, right) ->
        let synLeft = exprToAst left
        let synRight = exprToAst right
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, synLeft, synRight, range)

    | Sequential(left, right) ->
        let synLeft = exprToAst left
        let synRight = exprToAst right
        SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, true, synLeft, synRight, range)

    | TryWith(body, _, _, cv, cb) ->
        let synBody = exprToAst body
        let synPat = mkVarPat range cv
        let synCatch = exprToAst cb
        let synClause = SynMatchClause.Clause(synPat, None, synCatch, range, SequencePointInfoForTarget.SequencePointAtTarget)
        SynExpr.TryWith(synBody, range, [synClause], range, range, SequencePointInfoForTry.SequencePointAtTry range, SequencePointInfoForWith.SequencePointAtWith range)

    | TryFinally(body, finalizer) ->
        let synBody = exprToAst body
        let synFinalizer = exprToAst finalizer
        SynExpr.TryFinally(synBody, synFinalizer, range, SequencePointInfoForTry.SequencePointAtTry range, SequencePointInfoForFinally.SequencePointAtFinally range)

    | IfThenElse(cond, a, b) ->
        let synCond = exprToAst cond
        let synA = exprToAst a
        let synB = exprToAst b
        SynExpr.IfThenElse(synCond, synA, Some synB, SequencePointInfoForBinding.SequencePointAtBinding range, false, range, range)

    | WhileLoop(cond, body) ->
        let synCond = exprToAst cond
        let synBody = exprToAst body
        SynExpr.While(SequencePointAtWhileLoop range, synCond, synBody, range)

    | Coerce(e, t) ->
        let synExpr = exprToAst e
        let synType = sysTypeToSynType range t
        SynExpr.Upcast(synExpr, synType, range)

    | NewObject(ctorInfo, args) ->
        let synType = sysTypeToSynType range ctorInfo.DeclaringType
        let synArgs = List.map exprToAst args
        let synParam = SynExpr.Tuple(synArgs, [], range)
        SynExpr.New(false, synType, synParam, range)

    | NewTuple(args) ->
        let synArgs = List.map exprToAst args
        SynExpr.Tuple(synArgs, [], range)

    | NewUnionCase(uci, args) ->
        let uciCtor = mkUciCtor range uci
        let synArgs = List.map exprToAst args
        match synArgs with
        | [] -> uciCtor
        | [a] -> SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, a, range)
        | _ ->
            let synParam = SynExpr.Tuple(synArgs, [], range)
            SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, synParam, range)

    | UnionCaseTest(expr, uci) ->
        let synExpr = exprToAst expr
        let uciIdent = SynPat.LongIdent(mkUciIdent range uci, None, None, SynConstructorArgs.Pats [], None, range)
        let matchClause = SynMatchClause.Clause(uciIdent, None, SynExpr.Const(SynConst.Bool true, range0), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
        let notMatchClause = SynMatchClause.Clause(SynPat.Wild range0, None, SynExpr.Const(SynConst.Bool false, range0), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
        SynExpr.Match(SequencePointInfoForBinding.SequencePointAtBinding range, synExpr, [matchClause ; notMatchClause], false, range)

    | Call(instance, methodInfo, args) ->
        let synArgs = List.map exprToAst args |> List.toArray
        // TODO : need a way to identify F# 'generic values', i.e. typeof<'T>
        // it seems that the only way to do this is by parsing F# assembly signature metadata
        // for now, use a heuristic that happens to hold for FSharp.Core operators
        // but not user-defined values. These are not supported for now.
        let defaultGrouping =
            if Array.isEmpty synArgs && methodInfo.ContainsAttribute<RequiresExplicitTypeArgumentsAttribute> () then []
            else [synArgs.Length]

        let groupings = defaultArg (tryGetCurriedFunctionGroupings methodInfo) defaultGrouping
        let rec foldApp (funcExpr : SynExpr, i : int) (grouping : int) =
            let args =
                match grouping with
                | 0 -> SynExpr.Const(SynConst.Unit, range)
                | 1 -> synArgs.[i]
                | _ -> SynExpr.Paren(SynExpr.Tuple(Array.toList <| synArgs.[i .. i + grouping], [], range), range, None, range)

            let funcExpr2 = SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, args, range)
            funcExpr2, i + grouping

        let synMethod = 
            match instance with
            | None -> sysMemberToSynMember range methodInfo
            | Some inst ->
                let synInst = exprToAst inst
                let liwd = mkLongIdent range [mkIdent range methodInfo.Name]
                SynExpr.DotGet(synInst, range, liwd, range)

        let synMethod =
            if methodInfo.IsGenericMethod then
                let margs = methodInfo.GetGenericArguments() |> Seq.map (sysTypeToSynType range) |> Seq.toList
                SynExpr.TypeApp(synMethod, range, margs, [], None, range, range)
            else
                synMethod

        let callExpr,_ = List.fold foldApp (synMethod, 0) groupings
        callExpr

    | PropertyGet(instance, propertyInfo, []) ->
        match instance with
        | None -> sysMemberToSynMember range propertyInfo
        | Some inst ->
            let sysInst = exprToAst inst
            let liwd = mkLongIdent range [mkIdent range propertyInfo.Name]
            SynExpr.DotGet(sysInst, range, liwd, range)

    | PropertyGet(instance, propertyInfo, indexers) ->
        let synIndexer = 
            match List.map exprToAst indexers with
            | [one] -> SynIndexerArg.One(one)
            | synIdx -> SynIndexerArg.One(SynExpr.Tuple(synIdx, [range], range))

        match instance with
        | None -> 
            let ident = sysMemberToSynMember range propertyInfo.DeclaringType
            SynExpr.DotIndexedGet(ident, [synIndexer], range0, range0)
        | Some inst ->
            let synInst = exprToAst inst
            SynExpr.DotIndexedGet(synInst, [synIndexer], range, range)

    | PropertySet(instance, propertyInfo, [], value) ->
        let synValue = exprToAst value
        match instance with
        | None ->
            let ident = LongIdentWithDots(getMemberPath range propertyInfo, [])
            SynExpr.LongIdentSet(ident, synValue, range)
        | Some inst ->
            let synInst = exprToAst inst
            SynExpr.DotSet(synInst, LongIdentWithDots([mkIdent range propertyInfo.Name], []), synValue, range)

    | PropertySet(instance, propertyInfo, indexers, value) ->
        let synValue = exprToAst value
        let synIndexer = 
            match List.map exprToAst indexers with
            | [one] -> SynIndexerArg.One(one)
            | synIdx -> SynIndexerArg.One(SynExpr.Tuple(synIdx, [range], range))

        match instance with
        | None ->
            let ident = sysMemberToSynMember range propertyInfo.DeclaringType
            SynExpr.DotIndexedSet(ident, [synIndexer], synValue, range, range, range)

        | Some inst ->
            let synInst = exprToAst inst
            SynExpr.DotIndexedSet(synInst, [synIndexer], synValue, range, range, range)
        

    | AddressOf e -> notImpl expr
    | AddressSet(e,e') -> notImpl expr
    | FieldGet(inst, fieldInfo) -> notImpl expr
    | FieldSet(inst, fieldInfo, value) -> notImpl expr
    | ForIntegerRangeLoop(v, e, e', e'') -> notImpl expr

    | NewArray(t, e) -> notImpl expr
    | DefaultValue(t) -> notImpl expr
    | NewDelegate(t, vars, body) -> notImpl expr
    | NewRecord(t, exprs) -> notImpl expr
    | TupleGet(inst, idx) -> notImpl expr
    | TypeTest(expr, t) -> notImpl expr
    | VarSet(v, expr) -> notImpl expr
    | Quote e -> raise <| new NotSupportedException("nested quotations not supported")
    | _ -> notImpl expr

let synExprToLetBinding (expr : SynExpr) =
    let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
    let synPat = SynPat.LongIdent(mkLongIdent range0 [mkIdent range0 "compiledQuotation"], None, None, SynConstructorArgs.Pats [ SynPat.Paren(SynPat.Const(SynConst.Unit, range0), range0)], None, range0)
    let binding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, synPat, None, expr, range0, SequencePointInfoForBinding.SequencePointAtBinding range0)
    SynModuleDecl.Let(false, [binding], range0)


let letBindingToParsedInput (decl : SynModuleDecl) =
    let modl = SynModuleOrNamespace([mkIdent range0 "Test"], true, [decl], PreXmlDoc.Empty,[], None, range0)
    let file = ParsedImplFileInput("/test.fs", false, QualifiedNameOfFile(mkIdent range0 "Test"), [],[], [modl],false)
    ParsedInput.ImplFile file


let quotationToParsedInput : Expr -> ParsedInput = exprToAst >> synExprToLetBinding >> letBindingToParsedInput