module internal QuotationCompiler.Dependencies

open System
open System.Collections.Generic
open System.Reflection

/// Quotation assembly dependency aggregator
type DependencyContainer () =

    static let ignoredAssemblies =
        [| typeof<int> ; typeof<int option> |]
        |> Seq.map (fun t -> t.Assembly)
        |> HashSet

    let dependencies = new HashSet<Assembly> ()
    let loadedAssemblies = AppDomain.CurrentDomain.GetAssemblies()

    let rec append (assemblies : Assembly list) =
        match assemblies with
        | [] -> ()
        | a :: rest when dependencies.Contains a || ignoredAssemblies.Contains a -> append rest
        | a :: rest ->
            let _ = dependencies.Add a
            let deps = 
                a.GetReferencedAssemblies() 
                |> Seq.choose (fun an -> loadedAssemblies |> Array.tryFind(fun a -> a.FullName = an.FullName)) 
                |> Seq.toList

            append (deps @ rest)

    let rec appendType (t : Type) =
        append [t.Assembly]
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            for ga in t.GetGenericArguments() do 
                appendType ga

    /// Get resolved assembly dependencies
    member __.Assemblies = dependencies |> Seq.toList
    
    /// Append type to dependencies
    member __.Append(m : MemberInfo) =
        match m.DeclaringType with null -> () | dt -> appendType dt

        match m with
        | :? Type as t -> appendType t
        | :? ConstructorInfo as c ->
            appendType c.DeclaringType
            for a in c.GetParameters() do appendType a.ParameterType

        | :? MethodInfo as m ->
            appendType m.DeclaringType
            for ga in m.GetGenericArguments() do appendType ga
            for a in m.GetParameters() do appendType a.ParameterType

        | :? FieldInfo as f -> appendType f.FieldType
        | :? PropertyInfo as p -> appendType p.PropertyType
        | _ -> ()