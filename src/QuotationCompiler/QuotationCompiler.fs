namespace QuotationCompiler

open System
open System.Reflection
open System.IO

open Microsoft.FSharp.Quotations
    
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

type QuotationCompiler =

    /// <summary>
    ///     Converts supplied quotation tree to untyped AST.
    /// </summary>
    /// <param name="expr">Quotation to be converted.</param>
    /// <returns>Untyped AST and assembly dependencies.</returns>
    static member ToParsedInput(expr : #Expr) : Assembly list * ParsedInput = Transformer.convertExprToAst expr

    /// <summary>
    ///     Compiles provided quotation tree to assembly.
    /// </summary>
    /// <param name="expr">Quotation to be compiled.</param>
    /// <param name="targetDirectory">Target directory. Defaults to system temp folder.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    static member ToAssembly(expr : #Expr, ?targetDirectory : string, ?assemblyName : string) : string =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let targetDirectory = match targetDirectory with None -> Path.GetTempPath() | Some p -> p
        let assemblies, ast = Transformer.convertExprToAst expr
        let dependencies = assemblies |> List.map (fun a -> a.Location)
        let location = Path.Combine(targetDirectory, assemblyName + ".dll")
        let sscs = new SimpleSourceCodeServices()
        let errors, code = sscs.Compile([ast], assemblyName, location, dependencies, executable = false)
        if code = 0 then location
        else
            failwithf "Compilation failed with errors %A." errors

    /// <summary>
    ///     Compiles provided quotation tree to dynamic assembly.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    static member ToDynamicAssembly(expr : #Expr, ?assemblyName : string) : Assembly =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let assemblies, ast = Transformer.convertExprToAst expr
        let dependencies = assemblies |> List.map (fun a -> a.Location)
        let sscs = new SimpleSourceCodeServices()
        match sscs.CompileToDynamicAssembly([ast], assemblyName, dependencies, None, debug = false) with
        | _, _, Some a -> a
        | errors, _, _ -> failwithf "Compilation failed with errors %A." errors

    /// <summary>
    ///     Compiles provided quotation tree to function.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    static member ToFunc(expr : Expr<'T>) : unit -> 'T =
        let dynAss = QuotationCompiler.ToDynamicAssembly expr
        let methodInfo = dynAss.GetType(Transformer.moduleName).GetMethod(Transformer.compiledFunctionName)
        if typeof<'T> = typeof<unit> then
            let action = wrapDelegate<Action>(methodInfo)
            fun () -> action.Invoke() ; Unchecked.defaultof<'T>
        else
            let func = wrapDelegate<Func<'T>>(methodInfo)
            func.Invoke

    /// <summary>
    ///     Compiles provided quotation tree to value.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    static member Eval(expr : Expr<'T>) : 'T =
        let dynAss = QuotationCompiler.ToDynamicAssembly expr
        let methodInfo = dynAss.GetType(Transformer.moduleName).GetMethod(Transformer.compiledFunctionName)
        methodInfo.Invoke(null, [||]) :?> 'T

#if DEBUG
module Ast =

    /// <summary>
    ///     Parses source code string into untyped assembly.
    /// </summary>
    /// <param name="source">F# code to be parsed.</param>
    let ofSourceString (source : string) : ParsedInput option = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let checker = FSharpChecker.Create()
            let! options = checker.GetProjectOptionsFromScript(fileName, "")
            let! parsed = checker.ParseFileInProject(fileName, source, options)
            return parsed.ParseTree
        })
#endif