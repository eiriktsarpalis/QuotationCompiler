// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "FakeLib.dll"

open System
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
open Fake.DotNetCli

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "QuotationCompiler"
let summary = "An F# quotation compilation library that uses FSharp.Compiler.Service"

let gitHome = "https://github.com/eiriktsarpalis"
let gitName = "QuotationCompiler"
let gitOwner = "eiriktsarpalis"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/" + gitOwner

let configuration = environVarOrDefault "Configuration" "Release"
let artifactsFolder = __SOURCE_DIRECTORY__ @@ "artifacts"
let nugetProjects = !! "src/QuotationCompiler/*.??proj"

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

    let getProjectDetails (projectPath : string) =
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

Target "Build" (fun _ ->
    DotNetCli.Build(fun p ->
        { p with
            Project = __SOURCE_DIRECTORY__
            Configuration = configuration
            AdditionalArgs = [ yield sprintf "-p:Version=%O" release.NugetVersion ]
        })
)


// --------------------------------------------------------------------------------------
// Run the unit tests

Target "RunTests" (fun _ ->
    DotNetCli.Test (fun c ->
        { c with
            Project = __SOURCE_DIRECTORY__
            Configuration = configuration }))

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

//--------------------------------------------
// Release Targets

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = artifactsFolder }))

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "ReleaseGithub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    //StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "OctokitToken" with
        | null -> 
            let user =
                match getBuildParam "github-user" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserInput "Username: "
            let pw =
                match getBuildParam "github-pw" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserPassword "Password: "

            createClient user pw
        | token -> createClientWithToken token

    // release on github
    client
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "Default" DoNothing
Target "Bundle" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "NuGet"
  ==> "Bundle"

"Bundle"
  ==> "NuGetPush"
  ==> "ReleaseGithub"
  ==> "Release"

RunTargetOrDefault "Default"