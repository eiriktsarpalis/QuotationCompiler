module internal QuotationCompiler.Dependencies

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Reflection

/// Quotation assembly dependency aggregator
type DependencyContainer () =

    static let ignoredAssemblies =
        [| typeof<int> ; typeof<int option> |]
        |> Seq.map (fun (t:Type) -> t.Assembly)
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

// Creates a dummy F# console application to probe the sdk for the default set of shared framework assembly dependencies
// Adapted from https://github.com/dotnet/fsharp/blob/b4cc1c898314160094103dab34d8bab4e38ef8a3/tests/FSharp.Test.Utilities/CompilerAssert.fs
[<Sealed; AbstractClass>]
type SharedFrameworkDependencyResolver private () =

    static let getNetCoreAppReferences () =
        let projectDirectory = Path.Combine(Path.GetTempPath(), "CompilerAssert", Path.GetRandomFileName())
        let pathToFSharpCore = typeof<int option>.Assembly.Location
        let targetFramework = "net5.0"
        let projectFile = $"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>{targetFramework}</TargetFramework>
    <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
  </PropertyGroup>
  <ItemGroup><Compile Include="Program.fs" /></ItemGroup>
  <ItemGroup><Reference Include="{pathToFSharpCore}" /></ItemGroup>
  <Target Name="WriteFrameworkReferences" AfterTargets="AfterBuild">
    <WriteLinesToFile File="FrameworkReferences.txt" Lines="@(ReferencePath)" Overwrite="true" WriteOnlyWhenDifferent="true" />
  </Target>
</Project>
"""

        let programFs = """
open System
[<EntryPoint>]
let main argv = 0
"""     
        try
            try
                Directory.CreateDirectory(projectDirectory) |> ignore
                let projectFileName = Path.Combine(projectDirectory, "ProjectFile.fsproj")
                let programFsFileName = Path.Combine(projectDirectory, "Program.fs")
                let frameworkReferencesFileName = Path.Combine(projectDirectory, "FrameworkReferences.txt")
                File.WriteAllText(projectFileName, projectFile)
                File.WriteAllText(programFsFileName, programFs)

                let pInfo = 
                    ProcessStartInfo(
                        FileName = "dotnet",
                        Arguments = "build",
                        WorkingDirectory = projectDirectory,
                        RedirectStandardOutput = true,
                        RedirectStandardInput = true,
                        UseShellExecute = false)

                let p = Process.Start(pInfo)
                
                if not (p.WaitForExit(30000)) then
                    let output = p.StandardOutput.ReadToEnd()
                    let errors = p.StandardError.ReadToEnd()

                    failwith $"Failed to resolve shared framework references:\n{output}\n{errors}"

                File.ReadLines(frameworkReferencesFileName) |> Seq.toList
            with e ->
                failwith $"Failed to resolve shared framework references"
        finally
            try Directory.Delete(projectDirectory, true) with | _ -> ()

    static let sharedFrameworkDependencies = lazy(
        if isDotNetFramework then []
        else getNetCoreAppReferences()
    )

    static member Dependencies = sharedFrameworkDependencies.Value