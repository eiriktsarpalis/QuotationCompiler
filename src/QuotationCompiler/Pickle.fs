module internal QuotationCompiler.Pickle

    //
    //  Pickling support for values captured in quotations; work in progress
    //

    open System
    open System.IO
    open System.Collections.Generic
    open System.Reflection
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range

    type private Pickler =

        static member Pickle (value : obj) = 
            let bfs = new BinaryFormatter()
            use mem = new MemoryStream()
            bfs.Serialize(mem, value)
            mem.ToArray()

        static member SynUnPickler : SynExpr = notImpl "deserializer"

        static member SynCache (t : Type) (obj : obj) : SynExpr = 
            let synTy = sysTypeToSynType range0 t
            let pickle = Pickler.Pickle obj
            let synPickle = SynExpr.Const(SynConst.Bytes(pickle, range0), range0)
            let synDeserializer = Pickler.SynUnPickler
            let synDeserialize = SynExpr.App(ExprAtomicFlag.NonAtomic, false, synDeserializer, synPickle, range0)
            SynExpr.Typed(synDeserialize, synTy, range0)

    type private InlineCacheEntry =
        {
            Ident : Ident
            Binding : SynModuleDecl
        }
    with
        static member Create(value : obj, ty : Type) =
            let id = sprintf "item_%s" <| Guid.NewGuid().ToString("N")
            let ident = mkIdent range0 id
            let synPat = SynPat.Named(SynPat.Wild range0, ident, false, Some SynAccess.Private, range0)
            let expr = Pickler.SynCache ty value
            let binding = SynModuleDecl.Let(false, [mkBinding range0 synPat expr], range0)
            {
                Ident = ident
                Binding = binding
            }


    type PickleManager() =
        let idGen = new ObjectIDGenerator()
        let container = new Dictionary<int64, InlineCacheEntry> ()

        member __.Append(obj:obj, ty : Type) =
            let id, isFirst = idGen.GetId obj
            if isFirst then
                let entry = InlineCacheEntry.Create(obj, ty)
                container.Add(id, entry)
                entry.Ident
            else
                let entry = container.[id] in entry.Ident

        member __.Bindings = container |> Seq.map(function (KeyValue(_,v)) -> v.Binding) |> Seq.toList