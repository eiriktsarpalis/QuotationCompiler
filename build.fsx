// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "FakeLib.dll"

open System
open System.IO
open Fake.AppVeyor
open Fake 
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
open Fake.Testing
open Fake.DotNetCli

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "QuotationCompiler"
let summary = "An F# quotation compilation library that uses FSharp.Compiler.Service"

let gitHome = "https://github.com/eiriktsarpalis"
let gitName = "QuotationCompiler"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/eiriktsarpalis"

let configuration = environVarOrDefault "Configuration" "Release"
let artifactsFolder = __SOURCE_DIRECTORY__ @@ "artifacts"
let nugetProjects = !! "src/QuotationCompiler/**.??proj"
let testProjects = !! "tests/**.??proj"

//
// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs <| !! "./**/bin/"
    CleanDir "./tools/output"
    CleanDir "./temp"
)

//
// --------------------------------------------------------------------------------------
// Build library & test project

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title projectName
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ())
)

Target "DotNet.Restore" (fun _ -> DotNetCli.Restore id)

Target "Build" (fun _ ->
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration; "SourceLinkCreate", "true"]
    |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests

let runTests config (proj : string) =
    if EnvironmentHelper.isWindows then
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = config })
    else
        // work around xunit/mono issue
        let projDir = Path.GetDirectoryName proj
        let projName = Path.GetFileNameWithoutExtension proj
        let netcoreFrameworks, legacyFrameworks = 
            !! (projDir @@ "bin" @@ config @@ "*/")
            |> Seq.map Path.GetFileName
            |> Seq.toArray
            |> Array.partition 
                (fun f -> 
                    f.StartsWith "netcore" || 
                    f.StartsWith "netstandard")

        for framework in netcoreFrameworks do
            DotNetCli.Test (fun c ->
                { c with
                    Project = proj
                    Framework = framework
                    Configuration = config })

        for framework in legacyFrameworks do
            let assembly = projDir @@ "bin" @@ config @@ framework @@ projName + ".dll"
            !! assembly
            |> xUnit2 (fun c ->
                { c with
                    Parallel = ParallelMode.Collections
                    TimeOut = TimeSpan.FromMinutes 20. })

Target "RunTests" (fun _ ->
    for proj in testProjects do
        runTests "Release" proj)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    for proj in nugetProjects do
        DotNetCli.Pack(fun p ->
            { p with
                Configuration = configuration
                Project = proj
                AdditionalArgs = 
                    [ yield "--no-build" ; 
                      yield "--no-dependencies" ; 
                      yield sprintf "-p:Version=%O" release.NugetVersion ]
                OutputPath = artifactsFolder
            })
)

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = artifactsFolder }))


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "Default" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "DotNet.Restore"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Build"
  ==> "NuGet"
  ==> "Release"

"NuGet" 
  ==> "NuGetPush"

RunTargetOrDefault "Default"