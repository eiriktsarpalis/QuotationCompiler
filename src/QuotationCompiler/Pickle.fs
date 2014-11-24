module internal QuotationCompiler.Pickle

//
//  Provides a mechanism in which quotation values are captured
//  in the form of top-level cached bindings
//

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module private Pickler =

    let pickle (value : obj) : byte [] =
        let bfs = new BinaryFormatter()
        use mem = new MemoryStream()
        bfs.Serialize(mem, value)
        mem.ToArray()

    let unPickleExpr(pickle : byte[], t : Type) =
        let untypedExpr =
            <@
                let bfs = new BinaryFormatter()
                use mem = new MemoryStream(pickle)
                bfs.Deserialize(mem)
            @>

        Expr.Coerce(untypedExpr, t)

/// Represents a cached value instance
type CachedValue =
    {
        Ident : Ident
        UnPickleExpr : Expr
    }
with
    static member Create(value : obj, ty : Type) =
        let ident = mkUniqueIdentifier range0
        let pickle = Pickler.pickle value
        let expr = Pickler.unPickleExpr(pickle, ty)
        { Ident = ident ; UnPickleExpr = expr }

/// Manages serialized Expr.Value bindings
type PickleManager() =
    let idGen = new ObjectIDGenerator()
    let container = new Dictionary<int64, CachedValue> ()

    /// Caches value and returns unique identifier for instance
    member __.Append(obj:obj, ty : Type) =
        let id, isFirst = idGen.GetId obj
        if isFirst then
            let entry = CachedValue.Create(obj, ty)
            container.Add(id, entry)
            entry.Ident
        else
            let entry = container.[id] in entry.Ident

    /// Returns all cached values
    member __.CachedValues = container |> Seq.map(function (KeyValue(_,v)) -> v) |> Seq.toList