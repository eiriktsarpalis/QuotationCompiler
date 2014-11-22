module internal QuotationCompiler.Dependencies

open System
open System.Reflection

/// Quotation assembly dependency aggregator
type Dependencies =
    {
        Index : Map<string, Assembly>
    }
with
    static member Empty = { Index = Map.empty }

    member __.Assemblies = __.Index |> Map.toSeq |> Seq.map snd |> Seq.toList

    member __.Append(t : Type) =
        let assemblies = ref __.Index
        let append (assembly : Assembly) =
            if assemblies.Value.ContainsKey assembly.FullName then () else
            if assembly.IsDynamic then 
                raise <| new NotSupportedException(sprintf "Quotation depends on dynamic assembly %s." assembly.FullName)

            elif assembly = typeof<int>.Assembly || assembly = typeof<int option>.Assembly then ()
            else
                assemblies := Map.add assembly.FullName assembly !assemblies

        let rec traverse (t : Type) =
            append t.Assembly
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                for ga in t.GetGenericArguments() do 
                    traverse ga

        traverse t
        { Index = !assemblies }