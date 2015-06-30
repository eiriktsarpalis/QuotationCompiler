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

open QuotationCompiler.Utils

/// Expression-based value serialization abstraction
type IExprSerializer =
    /// Produces an expression tree that performs
    /// deserialization for provided value at runtime.
    abstract Pickle<'T> : 'T -> Expr<'T>

/// Represents a value that has been pickled as expression
type ExprPickle =
    {
        /// Identifier used for referencing the value.
        Ident : Ident
        /// Expression constructing the value.
        Expr : Expr
    }

/// Manages serialized Expr.Value bindings
type PickledValueManager(serializer : IExprSerializer) =
    let idGen = new ObjectIDGenerator()
    let container = new Dictionary<int64, ExprPickle> ()

    /// Caches value and returns unique identifier for instance
    member __.Append(obj:obj, ty : Type) =
        let id, isFirst = idGen.GetId obj
        if isFirst then
            let ident = mkUniqueIdentifier range0
            let e = Existential.FromType ty
            let expr = e.Apply { new IFunc<Expr> with member __.Invoke<'T>() = serializer.Pickle<'T>(unbox obj) :> Expr }
            container.Add(id, { Ident = ident ; Expr = expr })
            ident
        else
            let entry = container.[id] in entry.Ident

    /// Returns all cached values
    member __.PickledValues = container |> Seq.map(function (KeyValue(_,v)) -> v) |> Seq.toList


type BinaryFormatterExprSerializer() =
    interface IExprSerializer with
        member __.Pickle<'T>(value : 'T) : Expr<'T> =
            let bfs = new BinaryFormatter()
            use mem = new MemoryStream()
            bfs.Serialize(mem, value)
            // ensure value is deserializable at compile time
            mem.Position <- 0L
            let _ = bfs.Deserialize(mem)
            // create unpickle expr
            let pickle = mem.ToArray()
            <@
                let bfs = new BinaryFormatter()
                use mem = new MemoryStream(pickle)
                bfs.Deserialize(mem) :?> 'T
            @>