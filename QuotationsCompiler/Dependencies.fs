module internal QuotationsCompiler.Dependencies

open System
open System.Reflection

type Dependencies =
    {
        Index : Map<string, Assembly>
    }
with
    static member Empty = { Index = Map.empty }

    member __.Assemblies = __.Index |> Map.toSeq |> Seq.map snd |> Seq.toList

    member __.Append(t : Type) =
        let assemblies = ref __.Index

        let rec traverse (t : Type) =
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                for ga in t.GetGenericArguments() do 
                    traverse ga
            else
                let a = t.Assembly
                if a.IsDynamic then raise <| new NotSupportedException(sprintf "Quotation depends on dynamic assembly %s." a.FullName)
                elif a = typeof<int>.Assembly || a = typeof<int option>.Assembly then ()
                else
                    assemblies := Map.add a.FullName a !assemblies

        traverse t
        { Index = !assemblies }