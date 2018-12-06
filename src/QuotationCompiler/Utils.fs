namespace QuotationCompiler

open System
open System.Text.RegularExpressions
open System.Reflection
open System.Runtime.Serialization

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
    
[<AutoOpen>]
module internal Utils =

    let inline notImpl<'T> e : 'T = raise <| new NotImplementedException(sprintf "%O" e)

    let isDotNetFramework = typeof<int>.Assembly.GetName().Name = "mscorlib"

    let isListType (t : Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>

    let isOptionType (t : Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    let wrapDelegate<'Dele when 'Dele :> Delegate> (m : MethodInfo) =
        Delegate.CreateDelegate(typeof<'Dele>, m) :?> 'Dele

    /// taken from mscorlib's Tuple.GetHashCode() implementation
    let inline private combineHash (h1 : int) (h2 : int) =
        ((h1 <<< 5) + h1) ^^^ h2

    /// pair hashcode generation without tuple allocation
    let inline hash2 (t : 'T) (s : 'S) =
        combineHash (hash t) (hash s)
        
    /// triple hashcode generation without tuple allocation
    let inline hash3 (t : 'T) (s : 'S) (u : 'U) =
        combineHash (combineHash (hash t) (hash s)) (hash u)

    /// quadruple hashcode generation without tuple allocation
    let inline hash4 (t : 'T) (s : 'S) (u : 'U) (v : 'V) =
        combineHash (combineHash (combineHash (hash t) (hash s)) (hash u)) (hash v)

    type MemberInfo with
        member m.TryGetCustomAttribute<'Attr when 'Attr :> System.Attribute> () =
            m.GetCustomAttributes<'Attr> () |> Seq.tryHead

        member m.ContainsAttribute<'Attr when 'Attr :> System.Attribute> () =
            m.GetCustomAttributes<'Attr> () |> Seq.isEmpty |> not

        member m.Assembly = match m with :? Type as t -> t.Assembly | _ -> m.DeclaringType.Assembly

        member m.GetCompilationRepresentationFlags() =
            match m.TryGetCustomAttribute<CompilationRepresentationAttribute>() with
            | None -> CompilationRepresentationFlags.None
            | Some attr -> attr.Flags

    type MethodBase with
        member m.GetOptionalParameterInfo () =
            m.GetParameters()
            |> Seq.map (fun p -> 
                if p.GetCustomAttributes<OptionalArgumentAttribute>() |> Seq.isEmpty then None
                else
                    Some p.Name)
            |> Seq.toList

    /// build a range value parsing the Expr.CustomAttributes property.
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

    let inline mkBinding range isTopLevelValue pat expr =
        let synValData = SynValData.SynValData(None, SynValInfo((if isTopLevelValue then [] else [[]]), SynArgInfo([], false, None)), None)
        SynBinding.Binding(None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, synValData, pat, None, expr, range0, SequencePointInfoForBinding.SequencePointAtBinding range)

    let mkUniqueIdentifier range =
        let suffix = Guid.NewGuid().ToString("N")
        let name = "_bind_" + suffix
        mkIdent range name

    let private moduleSuffixRegex = new Regex(@"^(.*)Module$", RegexOptions.Compiled)
    let private fsharpPrefixRegex = new Regex(@"^FSharp(.*)(`[0-9]+)?$", RegexOptions.Compiled)
    /// recover the F# source name for given member declaration
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

    /// generate full path for given memberinfo
    let getMemberPath range (m : MemberInfo) =
        let rec aux (m : MemberInfo) = seq {
            match m.DeclaringType with
            | null -> 
                match (m :?> Type).Namespace with
                | null -> ()
                | ns -> yield! ns.Split('.') 
            | dt -> yield! aux dt 
        
            yield getFSharpName m
        }

        aux m |> Seq.map (mkIdent range) |> Seq.toList

    /// converts a System.Type to a F# compiler SynType expression
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

    /// creates a union case identifier
    let mkUciIdent range (uci : UnionCaseInfo) =
        let path = getMemberPath range uci.DeclaringType
        LongIdentWithDots(path @ [mkIdent range uci.Name], [range])

    /// recover curried function argument groupings for given method declaration
    let tryGetCurriedFunctionGroupings (m : MethodInfo) =
        match m.TryGetCustomAttribute<CompilationArgumentCountsAttribute> () with
        | None -> None
        | Some a -> Some(a.Counts |> Seq.toList)

    /// converts a System.Reflection.MemberInfo to an SynExpr identifier
    let sysMemberToSynMember range (m : MemberInfo) =
        let liwd = LongIdentWithDots(getMemberPath range m, [range])
        SynExpr.LongIdent(false, liwd, None, range)

    /// creates a syntactic argument passed to method calls
    let mkArgumentBinding range optName synParam =
        match optName with
        | None -> synParam
        | Some name -> 
            let equality = SynExpr.Ident(mkIdent range "op_Equality")
            let ident = SynExpr.LongIdent(true, mkLongIdent range [mkIdent range name], None, range)
            let innerApp = SynExpr.App(ExprAtomicFlag.NonAtomic, true, equality, ident, range)
            SynExpr.App(ExprAtomicFlag.NonAtomic, false, innerApp, synParam, range)

    /// recognizes bindings to union case fields in pattern matches
    /// Quotations directly access propertyInfo instances, which are
    /// not public. Recovers union metadata required for a proper pattern match expression
    /// in the F# ast.
    let (|UnionCasePropertyGet|_|) (expr : Expr) =
        match expr with
        | PropertyGet(Some instance, propertyInfo, []) when FSharpType.IsUnion instance.Type ->
            if isOptionType instance.Type then None
            elif isListType instance.Type then None
            // a property is a union case field \iff its declaring type is a proper subtype of the union type
            elif instance.Type = propertyInfo.DeclaringType then None
            else
                // create a dummy instance for declaring type to recover union case info
                let dummy = FormatterServices.GetUninitializedObject propertyInfo.DeclaringType
                let uci,_ = FSharpValue.GetUnionFields(dummy, propertyInfo.DeclaringType, true)
                match uci.GetFields() |> Array.tryFindIndex (fun p -> p = propertyInfo) with
                | Some i -> Some(instance, uci, i)
                | None -> None
        | _ -> None

    /// Type existential container
    [<AbstractClass>]
    type Existential internal () =
        static let genTy = typedefof<Existential<_>>

        /// System.Type representation of type
        abstract Type : Type
        /// Accepts a generic thunk to encapsulated type
        abstract Apply<'R> : IFunc<'R> -> 'R

        /// <summary>
        ///     Use reflection to initialize an encapsulated type.
        /// </summary>
        /// <param name="t"></param>
        static member FromType(t : Type) =
            let et = genTy.MakeGenericType [|t|]
            let ctor = et.GetConstructor [||]
            ctor.Invoke [||] :?> Existential

    /// Existential container of type 'T
    and [<Sealed; AutoSerializable(true)>] Existential<'T> () =
        inherit Existential()

        override __.Type = typeof<'T>
        override __.Apply<'R> (f : IFunc<'R>) = f.Invoke<'T> ()
        override __.Equals(other:obj) =
            match other with
            | :? Existential<'T> -> true
            | _ -> false

        override __.GetHashCode() = typeof<'T>.GetHashCode()

    /// Generic function
    and IFunc<'R> =
        abstract Invoke<'T> : unit -> 'R