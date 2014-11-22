module internal QuotationCompiler.Transformer

open System
open System.Reflection

open Microsoft.FSharp.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

open QuotationCompiler.Dependencies

[<Literal>]
let moduleName = "CompiledQuotationContainer"

[<Literal>]
let compiledFunctionName = "compiledQuotation"

/// Converts provided quotation to an untyped F# AST
let convertExprToAst (expr : Expr) =

    let dependencies = ref Dependencies.Empty
    let append (t : Type) = dependencies := dependencies.Value.Append t
    let defaultRange = defaultArg (tryParseRange expr) range0

    let rec exprToAst (expr : Expr) : SynExpr =
        let range = defaultArg (tryParseRange expr) defaultRange
        match expr with
        // parse for constants
        | Value(obj, t) ->
            match obj with
            | :? bool as b when t = typeof<bool> -> SynExpr.Const(SynConst.Bool b, range)
            | :? byte as b when t = typeof<byte> -> SynExpr.Const(SynConst.Byte b, range)
            | :? sbyte as b when t = typeof<sbyte> -> SynExpr.Const(SynConst.SByte b, range)
            | :? char as c when t = typeof<char> -> SynExpr.Const(SynConst.Char c, range)
            | :? decimal as d when t = typeof<decimal> -> SynExpr.Const(SynConst.Decimal d, range)
            | :? int16 as i when t = typeof<int16> -> SynExpr.Const(SynConst.Int16 i, range)
            | :? int32 as i when t = typeof<int32> -> SynExpr.Const(SynConst.Int32 i, range)
            | :? int64 as i when t = typeof<int64> -> SynExpr.Const(SynConst.Int64 i, range)
            | :? uint16 as i when t = typeof<uint16> -> SynExpr.Const(SynConst.UInt16 i, range)
            | :? uint32 as i when t = typeof<uint32> -> SynExpr.Const(SynConst.UInt32 i, range)
            | :? uint64 as i when t = typeof<uint64> -> SynExpr.Const(SynConst.UInt64 i, range)
            | :? IntPtr as i when t = typeof<IntPtr> -> SynExpr.Const(SynConst.IntPtr(int64 i), range)
            | :? UIntPtr as i when t = typeof<UIntPtr> -> SynExpr.Const(SynConst.UIntPtr(uint64 i), range)
            | :? single as f when t = typeof<single> -> SynExpr.Const(SynConst.Single f, range)
            | :? double as f when t = typeof<double> -> SynExpr.Const(SynConst.Double f, range)
            | :? string as s when t = typeof<string> -> SynExpr.Const(SynConst.String(s, range), range)
            | :? unit when t = typeof<unit> -> SynExpr.Const(SynConst.Unit, range)
            | :? (byte[]) as bs when t = typeof<byte[]> -> SynExpr.Const(SynConst.Bytes(bs, range), range)
            | :? (uint16[]) as is when t = typeof<uint16[]> -> SynExpr.Const(SynConst.UInt16s is, range)
            // null literal support
            | null when not <| t.GetCompilationRepresentationFlags().HasFlag CompilationRepresentationFlags.UseNullAsTrueValue ->
                let synTy = sysTypeToSynType range t
                SynExpr.Typed(SynExpr.Null range, synTy, range)
            | _ -> raise <| new NotSupportedException(sprintf "Quotation captures closure of type %O." t)

        | Var v ->
            append v.Type
            let ident = mkIdent range v.Name
            SynExpr.Ident ident
        
        | Lambda(v, body) ->
            append v.Type
            let vType = sysTypeToSynType range v.Type
            let spat = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
            let untypedPat = SynSimplePats.SimplePats([spat], range)
            let typedPat = SynSimplePats.Typed(untypedPat, vType, range)
            let bodyAst = exprToAst body
            SynExpr.Lambda(false, false, typedPat, bodyAst, range)

        | LetRecursive(bindings, body) ->
            let mkBinding (v : Var, bind : Expr) =
                append v.Type
                let vType = sysTypeToSynType range v.Type
                let untypedPat = mkVarPat range v
                let typedPat = SynPat.Typed(untypedPat, vType, range)
                let synBind = exprToAst bind
                mkBinding range typedPat synBind

            let bindings = List.map mkBinding bindings
            let synBody = exprToAst body
            SynExpr.LetOrUse(true, false, bindings, synBody, range)

        | Let(v, bind, body) ->
            append v.Type
            let vType = sysTypeToSynType range v.Type
            let untypedPat = mkVarPat range v
            let typedPat = SynPat.Typed(untypedPat, vType, range)
            let synBind = exprToAst bind
            let synBody = exprToAst body
            let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
            let synBinding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, v.IsMutable, [], PreXmlDoc.Empty, synValData, typedPat, None, synBind, range, SequencePointInfoForBinding.SequencePointAtBinding range)
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

        // Adapt F# exception constructors to F# idiomatic syntax
        | Coerce(NewObject(ctor, args), t) when t = typeof<exn> && FSharpType.IsExceptionRepresentation ctor.DeclaringType ->
            let exnTy = ctor.DeclaringType
            append exnTy
            let synExn = sysMemberToSynMember range exnTy
            match List.map exprToAst args with
            | [] -> synExn
            | [arg] -> SynExpr.App(ExprAtomicFlag.NonAtomic, false, synExn, arg, range)
            | args ->
                let paren = SynExpr.Tuple(args, [], range)
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, synExn, paren, range)

        | Coerce(e, t) ->
            append t
            let synExpr = exprToAst e
            let synType = sysTypeToSynType range t
            let synCoerce =
                if t.IsAssignableFrom e.Type then
                    SynExpr.Upcast(synExpr, synType, range)
                else
                    SynExpr.Downcast(synExpr, synType, range)

            SynExpr.Paren(synCoerce, range, None, range)

        | TypeTest(expr, t) ->
            append t
            let synExpr = exprToAst expr
            let synTy = sysTypeToSynType range t
            SynExpr.TypeTest(synExpr, synTy, range)

        | NewObject(ctorInfo, args) ->
            append ctorInfo.DeclaringType
            let synType = sysTypeToSynType range ctorInfo.DeclaringType
            let synParam =
                match List.map exprToAst args with
                | [] -> SynExpr.Const(SynConst.Unit, range)
                | [a] -> SynExpr.Paren(a, range, None, range)
                | synParams -> SynExpr.Tuple(synParams, [], range)

            SynExpr.New(false, synType, synParam, range)

        | NewTuple(args) ->
            let synArgs = List.map exprToAst args
            SynExpr.Tuple(synArgs, [], range)

        | NewArray(t, elems) ->
            append t
            let synTy = sysTypeToSynType range t
            let synArrayTy = SynType.Array(1, synTy, range)
            let synElems = List.map exprToAst elems
            let synArray = SynExpr.ArrayOrList(true, synElems, range)
            SynExpr.Typed(synArray, synArrayTy, range)

        | NewRecord(ty, entries) ->
            append ty
            let synTy = sysTypeToSynType range ty
            let fields = FSharpType.GetRecordFields(ty, BindingFlags.NonPublic ||| BindingFlags.Public) |> Array.toList
            let synEntries = List.map exprToAst entries
            let entries = (fields, synEntries) ||> List.map2 (fun f e -> (mkLongIdent range [mkIdent range f.Name], true), Some e, None)
            let synExpr = SynExpr.Record(None, None, entries, range)
            SynExpr.Typed(synExpr, synTy, range)

        | NewUnionCase(uci, args) ->
            append uci.DeclaringType
            let synTy = sysTypeToSynType range uci.DeclaringType
            let uciCtor = SynExpr.LongIdent(false, mkUciIdent range uci, None, range)
            let synArgs = List.map exprToAst args
            let ctorExpr =
                match synArgs with
                | [] -> uciCtor
                | [a] -> SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, a, range)
                | _ ->
                    let synParam = SynExpr.Tuple(synArgs, [], range)
                    SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, synParam, range)

            SynExpr.Typed(ctorExpr, synTy, range)

        | NewDelegate(t, vars, body) ->
            append t
            let synType = sysTypeToSynType range t
            let synBody = exprToAst body
            let rec mkLambda acc (rest : Var list) =
                match rest with
                | [] -> acc
                | v :: tail ->
                    let vType = sysTypeToSynType range v.Type
                    let spat = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
                    let untypedPat = SynSimplePats.SimplePats([spat], range)
                    let typedPat = SynSimplePats.Typed(untypedPat, vType, range)
                    let synLambda = SynExpr.Lambda(false, false, typedPat, acc, range)
                    mkLambda synLambda tail

            let synAbs = mkLambda synBody (List.rev vars)
            SynExpr.New(false, synType, SynExpr.Paren(synAbs, range, None, range), range)

        | UnionCaseTest(expr, uci) ->
            append uci.DeclaringType
            let synExpr = exprToAst expr
            let ctorPat =
                if isListType uci.DeclaringType then
                    // list pattern match requires special syntax
                    if uci.Name = "Empty" then
                        SynPat.ArrayOrList(false, [], range)
                    else // Cons
                        let uciIdent = mkLongIdent range [mkIdent range "op_ColonColon"]
                        let pats = SynPat.Tuple([SynPat.Wild range ; SynPat.Wild range], range)
                        SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.Pats [pats], None, range)
                else
                    let uciIdent = mkUciIdent range uci
                    let ctorArgs = if uci.GetFields().Length = 0 then [] else [SynPat.Wild range]
                    SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.Pats ctorArgs, None, range)

            let matchClause = SynMatchClause.Clause(ctorPat, None, SynExpr.Const(SynConst.Bool true, range), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
            let notMatchClause = SynMatchClause.Clause(SynPat.Wild range, None, SynExpr.Const(SynConst.Bool false, range), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
            SynExpr.Match(SequencePointInfoForBinding.SequencePointAtBinding range, synExpr, [matchClause ; notMatchClause], false, range)

        | Call(instance, methodInfo, args) ->
            append methodInfo.DeclaringType
            let synArgs = List.map exprToAst args |> List.toArray
            // TODO : need a way to identify F# 'generic values', i.e. typeof<'T>
            // it seems that the only way to do this is by parsing F# assembly signature metadata
            // for now, use a heuristic that happens to hold for FSharp.Core operators
            // but not user-defined values. These are not supported for now.
            let defaultGrouping =
                if Array.isEmpty synArgs && 
                    (methodInfo.ContainsAttribute<RequiresExplicitTypeArgumentsAttribute> ()
                        || methodInfo.ContainsAttribute<GeneralizableValueAttribute> ()) then []
                else [synArgs.Length]

            let groupings = defaultArg (tryGetCurriedFunctionGroupings methodInfo) defaultGrouping
            let rec foldApp (funcExpr : SynExpr, i : int) (grouping : int) =
                let args =
                    match grouping with
                    | 0 -> SynExpr.Const(SynConst.Unit, range)
                    | 1 -> SynExpr.Paren(synArgs.[i], range, None, range)
                    | _ -> SynExpr.Paren(SynExpr.Tuple(Array.toList <| synArgs.[i .. i + grouping - 1], [], range), range, None, range)

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

        | TupleGet(tuple, idx) ->
            let synTuple = exprToAst tuple
            let arity = FSharpType.GetTupleElements(tuple.Type).Length
            let ident = mkUniqueIdentifier range
            let synIdent = SynExpr.Ident(ident)
            let patterns = 
                [ 
                    for i in 0 .. idx - 1 -> SynPat.Wild range
                    yield SynPat.Named(SynPat.Wild range, ident, false, None, range)
                    for i in idx + 1 .. arity - 1 -> SynPat.Wild range
                ]

            let synPat = SynPat.Tuple(patterns, range)
            let binding = mkBinding range synPat synTuple
            SynExpr.LetOrUse(false, false, [binding], synIdent, range)

        | UnionCasePropertyGet(instance, isList, uci, prop, pos, fieldCount) ->
            append uci.DeclaringType
            let synInstance = exprToAst instance
            let synTy = sysTypeToSynType range prop.PropertyType
            let ident = mkUniqueIdentifier range
            let untypedPat = SynPat.Named(SynPat.Wild range, ident, false, None, range)
            let typedPat = SynPat.Typed(untypedPat, synTy, range)
            let patterns =
                [
                    for i in 0 .. pos - 1 -> SynPat.Wild range
                    yield typedPat
                    for i in pos + 1 .. fieldCount - 1 -> SynPat.Wild range
                ]

            let synPat = 
                match patterns with
                | [p] -> p
                | ps -> SynPat.Tuple(ps, range)

            let uciIdent =
                if isList then
                    mkLongIdent range [mkIdent range "op_ColonColon"]
                else
                    mkUciIdent range uci

            let matchPat = SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.Pats [synPat], None, range)
            let matchClause = SynMatchClause.Clause(matchPat, None, SynExpr.Ident ident, range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
            let synFailwith = SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(mkIdent range0 "failwith"), SynExpr.Const(SynConst.String("impossible", range0), range0), range0)
            let notMatchClause = SynMatchClause.Clause(SynPat.Wild range, None, synFailwith, range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
            SynExpr.Match(SequencePointInfoForBinding.SequencePointAtBinding range, synInstance, [matchClause ; notMatchClause], false, range)
            
        | PropertyGet(instance, propertyInfo, []) ->
            append propertyInfo.DeclaringType
            match instance with
            | None -> sysMemberToSynMember range propertyInfo
            | Some inst ->
                let sysInst = exprToAst inst
                let liwd = mkLongIdent range [mkIdent range propertyInfo.Name]
                SynExpr.DotGet(sysInst, range, liwd, range)

        | PropertyGet(instance, propertyInfo, indexers) ->
            append propertyInfo.DeclaringType
            let synIndexer = 
                match List.map exprToAst indexers with
                | [one] -> SynIndexerArg.One(one)
                | synIdx -> SynIndexerArg.One(SynExpr.Tuple(synIdx, [range], range))

            match instance with
            | None -> 
                let ident = sysMemberToSynMember range propertyInfo.DeclaringType
                SynExpr.DotIndexedGet(ident, [synIndexer], range, range)
            | Some inst ->
                let synInst = exprToAst inst
                SynExpr.DotIndexedGet(synInst, [synIndexer], range, range)

        | PropertySet(instance, propertyInfo, [], value) ->
            append propertyInfo.DeclaringType
            let synValue = exprToAst value
            match instance with
            | None ->
                let ident = LongIdentWithDots(getMemberPath range propertyInfo, [])
                SynExpr.LongIdentSet(ident, synValue, range)
            | Some inst ->
                let synInst = exprToAst inst
                SynExpr.DotSet(synInst, LongIdentWithDots([mkIdent range propertyInfo.Name], []), synValue, range)

        | PropertySet(instance, propertyInfo, indexers, value) ->
            append propertyInfo.DeclaringType
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

        | FieldGet(instance, fieldInfo) ->
            append fieldInfo.DeclaringType
            match instance with
            | None -> sysMemberToSynMember range fieldInfo
            | Some inst ->
                let synInst = exprToAst inst
                SynExpr.DotGet(synInst, range, mkLongIdent range [mkIdent range (getFSharpName fieldInfo)], range)

        | FieldSet(instance, fieldInfo, value) ->
            append fieldInfo.DeclaringType
            let synValue = exprToAst value
            match instance with
            | None ->
                let ident = LongIdentWithDots(getMemberPath range fieldInfo, [])
                SynExpr.LongIdentSet(ident, synValue, range)
            | Some inst ->
                let synInst = exprToAst inst
                SynExpr.DotSet(synInst, LongIdentWithDots([mkIdent range fieldInfo.Name], []), synValue, range)
        
        | VarSet(v, value) ->
            append v.Type
            let synValue = exprToAst value
            let synVar = LongIdentWithDots([mkIdent range v.Name], [])
            SynExpr.LongIdentSet(synVar, synValue, range)

        | ForIntegerRangeLoop(var, startExpr, endExpr, body) ->
            let varIdent = mkIdent range var.Name
            let synStartExpr = exprToAst startExpr
            let synEndExpr = exprToAst endExpr
            let synBody = exprToAst body
            SynExpr.For(SequencePointAtForLoop range, varIdent, synStartExpr, true, synEndExpr, synBody, range)

        | AddressOf e -> notImpl expr
        | AddressSet(e,e') -> notImpl expr
        | DefaultValue(t) -> notImpl expr
        | Quote e -> raise <| new NotSupportedException("nested quotations not supported")
        | _ -> notImpl expr

    let synExprToLetBinding (expr : SynExpr) =
        let synConsArgs = SynConstructorArgs.Pats [ SynPat.Paren(SynPat.Const(SynConst.Unit, defaultRange), defaultRange)]
        let synPat = SynPat.LongIdent(mkLongIdent defaultRange [mkIdent defaultRange compiledFunctionName], None, None, synConsArgs, None, defaultRange)
        // create a `let func () = () ; expr` binding to force return type compatible with quotation type.
        let seqExpr = SynExpr.Sequential(SequencePointsAtSeq, true, SynExpr.Const(SynConst.Unit, defaultRange), expr, defaultRange)
        let binding = mkBinding defaultRange synPat seqExpr
        SynModuleDecl.Let(false, [binding], defaultRange)

    let letBindingToParsedInput (decl : SynModuleDecl) =
        let modl = SynModuleOrNamespace([mkIdent defaultRange moduleName], true, [decl], PreXmlDoc.Empty,[], None, defaultRange)
        let file = ParsedImplFileInput("/QuotationCompiler.fs", false, QualifiedNameOfFile(mkIdent defaultRange moduleName), [],[], [modl],false)
        ParsedInput.ImplFile file

    let synExpr = exprToAst expr
    let parsedInput = synExpr |> synExprToLetBinding |> letBindingToParsedInput
    let assemblies = dependencies.Value.Assemblies
    assemblies, parsedInput