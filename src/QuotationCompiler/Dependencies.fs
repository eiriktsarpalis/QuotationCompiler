module internal QuotationCompiler.Dependencies

open System
open System.Collections.Generic
open System.Reflection

/// Quotation assembly dependency aggregator
type DependencyContainer () =
    let dependencies = new HashSet<Assembly> ()

    static let ignoredAssemblies =
        [| typeof<int> ; typeof<int option> |]
        |> Seq.map (fun t -> t.Assembly)
        |> hset

    let rec append (assemblies : Assembly list) =
        match assemblies with
        | [] -> ()
        | a :: rest when dependencies.Contains a || ignoredAssemblies.Contains a -> append rest
        | a :: _ when a.IsDynamic -> raise <| new NotSupportedException(sprintf "Quotation depends on dynamic assembly %O." a)
        | a :: rest ->
            let _ = dependencies.Add a
            let appDomainAssemblies = AppDomain.CurrentDomain.GetAssemblies()
            let deps = 
                a.GetReferencedAssemblies() 
                |> Seq.choose (fun an -> appDomainAssemblies |> Array.tryFind(fun a -> a.FullName = an.FullName)) 
                |> Seq.toList

            append (deps @ rest)

    let rec appendType (t : Type) =
        append [t.Assembly]
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            for ga in t.GetGenericArguments() do 
                appendType ga
    
    /// Append type to dependencies
    member __.Append(t : Type) = appendType t
    /// Get resolved assembly dependencies
    member __.Assemblies = dependencies |> Seq.toList