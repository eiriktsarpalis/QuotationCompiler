namespace QuotationCompiler

open System
open System.Reflection
open System.IO

open Microsoft.FSharp.Quotations
    
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

type QuotationCompiler =

    static member ToParsedInput(expr : #Expr) = Transformer.convertExprToAst expr

    static member ToAssembly(expr : #Expr, ?path : string, ?assemblyName : string) : string =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let path = match path with None -> Path.GetTempPath() | Some p -> p
        let assemblies, ast = Transformer.convertExprToAst expr
        let dependencies = assemblies |> List.map (fun a -> a.Location)
        let location = Path.Combine(path, assemblyName + ".dll")
        let sscs = new SimpleSourceCodeServices()
        let errors, code = sscs.Compile([ast], assemblyName, location, dependencies, executable = false)
        if code = 0 then location
        else
            failwithf "Compilation failed with errors %A." errors

    static member ToDynamicAssembly(expr : #Expr, ?assemblyName : string) : Assembly =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let assemblies, ast = Transformer.convertExprToAst expr
        let dependencies = assemblies |> List.map (fun a -> a.Location)
        let sscs = new SimpleSourceCodeServices()
        match sscs.CompileToDynamicAssembly([ast], assemblyName, dependencies, None, debug = false) with
        | _, _, Some a -> a
        | errors, _, _ -> failwithf "Compilation failed with errors %A." errors

    static member ToFunc(expr : Expr<'T>) : unit -> 'T =
        let dynAss = QuotationCompiler.ToDynamicAssembly expr
        let methodInfo = dynAss.GetType(Transformer.moduleName).GetMethod(Transformer.compiledFunctionName)
        if typeof<'T> = typeof<unit> then
            let action = wrapDelegate<Action>(methodInfo)
            fun () -> action.Invoke() ; Unchecked.defaultof<'T>
        else
            let func = wrapDelegate<Func<'T>>(methodInfo)
            func.Invoke

module Ast =

    let ofSourceString (source : string) = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let checker = FSharpChecker.Create()
            let! options = checker.GetProjectOptionsFromScript(fileName, "")
            let! parsed = checker.ParseFileInProject(fileName, source, options)
            return parsed.ParseTree
        })