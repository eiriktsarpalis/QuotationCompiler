namespace QuotationCompiler

    open System
    open System.Text.RegularExpressions
    open System.Reflection

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    
    [<AutoOpen>]
    module internal Utils =

        let inline notImpl<'T> e : 'T = raise <| new NotImplementedException(sprintf "%O" e)

        let wrapDelegate<'Dele when 'Dele :> Delegate> (m : MethodInfo) =
            Delegate.CreateDelegate(typeof<'Dele>, m) :?> 'Dele

        type MemberInfo with
            member m.TryGetCustomAttribute<'Attr when 'Attr :> System.Attribute> () =
                let attrs = m.GetCustomAttributes<'Attr> ()
                if Seq.isEmpty attrs then None
                else
                    Some(Seq.head attrs)

            member m.ContainsAttribute<'Attr when 'Attr :> System.Attribute> () =
                m.GetCustomAttributes<'Attr> () |> Seq.isEmpty |> not

            member m.Assembly = match m with :? Type as t -> t.Assembly | _ -> m.DeclaringType.Assembly

        let tryParseRange (expr : Expr) =
            match expr.CustomAttributes with
            | [ NewTuple [_; NewTuple [ Value (:? string as file, _); 
                                        Value (:? int as r1, _); Value (:? int as c1, _); 
                                        Value (:? int as r2, _); Value(:? int as c2, _)]]] -> 
                let p1 = mkPos r1 c1
                let p2 = mkPos r2 c2
                Some <| mkRange file p1 p2
            | _ -> None

        let inline mkIdent range text = new Ident(text, range)
        let inline mkLongIdent range (li : Ident list) = LongIdentWithDots(li, [range])
        let inline mkVarPat range (v : Quotations.Var) = 
            let lident = mkLongIdent range [mkIdent range v.Name]
            SynPat.LongIdent(lident, None, None, SynConstructorArgs.Pats [], None, range)

        let inline mkBinding range pat expr =
            let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
            SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, pat, None, expr, range0, SequencePointInfoForBinding.SequencePointAtBinding range)

        let private moduleSuffixRegex = new Regex(@"^(.*)Module$", RegexOptions.Compiled)
        let private fsharpPrefixRegex = new Regex(@"^FSharp(.*)(`[0-9]+)?$", RegexOptions.Compiled)
        let getFSharpName (m : MemberInfo) =
            match m.TryGetCustomAttribute<CompilationSourceNameAttribute> () with
            | Some a -> a.SourceName
            | None ->

            // this is a hack; need a better solution in the long term
            // see https://visualfsharp.codeplex.com/workitem/177
            if m.Assembly = typeof<int option>.Assembly && fsharpPrefixRegex.IsMatch m.Name then
                let rm = fsharpPrefixRegex.Match m.Name
                rm.Groups.[1].Value
            elif m.Name = "DefaultAsyncBuilder" && m.Assembly = typeof<int option>.Assembly then
                "async"
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

        let isListType (t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>

        let isOptionType (t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

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

        let (|UnionCasePropertyGet|_|) (expr : Expr) =
            match expr with
            | PropertyGet(Some inst, propertyInfo, []) when FSharpType.IsUnion propertyInfo.DeclaringType ->
                if isOptionType propertyInfo.DeclaringType then None
                else
                    let isList = isListType propertyInfo.DeclaringType
                    let uci = 
                        if isList then
                            FSharpType.GetUnionCases(propertyInfo.DeclaringType).[1]
                        else
                            let instance = System.Runtime.Serialization.FormatterServices.GetUninitializedObject propertyInfo.DeclaringType
                            let uci,_ = FSharpValue.GetUnionFields(instance, propertyInfo.DeclaringType, true)
                            uci

                    let flds = uci.GetFields()
                    assert(flds.Length > 0)
                    match flds |> Array.tryFindIndex (fun p -> p = propertyInfo) with
                    | Some i -> Some(inst, isList, uci, propertyInfo, i, flds.Length)
                    | None -> None
            | _ -> None