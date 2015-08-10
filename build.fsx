// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open System.IO
open Fake.AppVeyor
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "QuotationCompiler"

let gitHome = "https://github.com/eiriktsarpalis"
let gitName = "QuotationCompiler"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/nessos"



let testAssemblies = "bin/Release/*Tests*.dll"
//
//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")
let nugetVersion = release.NugetVersion


// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs <| !! "./**/bin/"
    CleanDir "./tools/output"
    CleanDir "./temp"
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

let configuration = environVarOrDefault "Configuration" "Release"

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let attrs =
        [ 
            Attribute.Version release.AssemblyVersion
            Attribute.FileVersion release.AssemblyVersion
        ] 

    CreateFSharpAssemblyInfo "src/QuotationCompiler/AssemblyInfo.fs" attrs
)


Target "Build" (fun _ ->
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuildRelease "" "Rebuild"
    |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target "RunTests" (fun _ ->
    try 
        !! testAssemblies
        |> NUnit (fun p ->
            { p with
                Framework = "v4.0"
                DisableShadowCopy = true
                TimeOut = TimeSpan.FromMinutes 20.
                OutputFile = "bin/TestResults.xml" })
    finally
        AppVeyor.UploadTestResultsXml AppVeyor.TestResultsType.NUnit "bin"
)

//// --------------------------------------------------------------------------------------
//// Build a NuGet package

Target "NuGet" (fun _ ->    
    Paket.Pack (fun p -> 
        { p with 
            ToolPath = ".paket/paket.exe" 
            OutputPath = "bin/"
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes })
)

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin/" }))


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "Default" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Build"
  ==> "NuGet"
  ==> "Release"

"NuGet" 
  ==> "NuGetPush"

RunTargetOrDefault "Default"