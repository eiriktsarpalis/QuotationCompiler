namespace QuotationCompiler

open System
open System.Runtime.Serialization

[<Serializable>]
type QuotationCompilerException =
    inherit Exception

    internal new (message : string, ?inner : exn) =
        { inherit Exception(message, defaultArg inner null) }

    private new (si : SerializationInfo, sc : StreamingContext) =
        { inherit Exception(si,sc) }