﻿namespace QuotationCompiler.Utilities

open System
open System.Reflection
open System.IO

open Microsoft.FSharp.Quotations
    
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open QuotationCompiler

/// Contains the result of a quotation converted to F# AST
type QuotationAst =
    {
        /// Syntax tree containing the converted quotation
        Tree : ParsedInput
        /// Assembly dependencies of F# Quotation
        Dependencies : Assembly list
        /// F# module containing converted quotation
        ModuleName : string
        /// F# function name defining converted quotation
        FunctionName : string
    }

type QuotationCompiler =

    /// <summary>
    ///     Converts supplied quotation tree to F# AST.
    /// </summary>
    /// <param name="expr">Quotation to be converted.</param>
    /// <param name="compiledModuleName">Name of compiled module containing AST.</param>
    /// <param name="compiledFunctionName">Name of compiled function name containing AST.</param>
    /// <param name="serializer">Serializer used for pickling values spliced into expression trees. Defaults to BinaryFormatter.</param>
    /// <returns>Untyped AST and assembly dependencies.</returns>
    static member ToParsedInput(expr : Expr, ?compiledModuleName : string, ?compiledFunctionName : string, ?serializer : IExprSerializer) : QuotationAst =
        let compiledModuleName =
            match compiledModuleName with
            | None -> sprintf "CompiledQuotationModule-%O" <| Guid.NewGuid()
            | Some cmn -> cmn

        let serializer = match serializer with Some s -> s | None -> new BinaryFormatterExprSerializer() :> IExprSerializer
        let compiledFunctionName = defaultArg compiledFunctionName "compiledQuotation"
        let dependencies, ast = Compiler.convertExprToAst serializer compiledModuleName compiledFunctionName expr
        {
            Tree = ast
            Dependencies = dependencies
            ModuleName = compiledModuleName
            FunctionName = compiledFunctionName
        }

#if DEBUG
    /// <summary>
    ///     Parses source code string into untyped assembly.
    /// </summary>
    /// <param name="source">F# code to be parsed.</param>
    static member ParseFSharpSource(source : string) : ParsedInput option = 
        Async.RunSynchronously(async {
            let fileName = "/mock.fs"
            let checker = FSharpChecker.Create()
            let! projOptions,_ = checker.GetProjectOptionsFromScript(fileName, "")
            let options,_ = checker.GetParsingOptionsFromProjectOptions(projOptions)
            let! parsed = checker.ParseFile(fileName, source, options)
            return parsed.ParseTree
        })
#endif

namespace QuotationCompiler

open System
open System.Reflection
open System.Collections.Concurrent
open System.IO

open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open QuotationCompiler.Utilities

type QuotationCompiler private () =

    /// Memoized compiled expression trees
    static let compiledExprs = new ConcurrentDictionary<Expr, obj>(new ExprEqualityComparer())

    static let printErrors (errors : FSharpErrorInfo []) =
        if Array.isEmpty errors then
            sprintf "Compilation failed with errors."
        else
            let errorMsgs = errors |> Seq.map (fun e -> sprintf "   ** %O" e) |> String.concat Environment.NewLine
            sprintf "Compilation failed with errors:%s%s" Environment.NewLine errorMsgs

    /// <summary>
    ///     Compiles provided quotation tree to assembly.
    /// </summary>
    /// <param name="expr">Quotation to be compiled.</param>
    /// <param name="targetDirectory">Target directory. Defaults to system temp folder.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    /// <param name="compiledModuleName">Name of compiled module containing AST.</param>
    /// <param name="compiledFunctionName">Name of compiled function name containing AST.</param>
    static member ToAssembly(expr : Expr, ?targetDirectory : string, ?assemblyName : string, 
                                            ?compiledModuleName : string, ?compiledFunctionName : string) : string =

        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let targetDirectory = match targetDirectory with None -> Path.GetTempPath() | Some p -> p
        let qast = QuotationCompiler.ToParsedInput(expr, ?compiledModuleName = compiledModuleName, ?compiledFunctionName = compiledFunctionName)
        let dependencies = qast.Dependencies |> List.map (fun a -> a.Location)
        let location = Path.Combine(targetDirectory, assemblyName + ".dll")
        let pdbFile = Path.Combine(targetDirectory, assemblyName + ".pdb")
        let sscs = FSharpChecker.Create()
        let errors, code = sscs.Compile([qast.Tree], assemblyName, location, dependencies, executable = false, pdbFile = pdbFile) |> Async.RunSynchronously
        if code = 0 then location
        else
            raise <| new QuotationCompilerException(printErrors errors)

    /// <summary>
    ///     Compiles provided quotation tree to dynamic assembly.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="assemblyName">Assembly name. Defaults to auto generated name.</param>
    static member ToDynamicAssembly(expr : Expr, ?assemblyName : string) : MethodInfo =
        let assemblyName = match assemblyName with None -> sprintf "compiledQuotation_%s" (Guid.NewGuid().ToString("N")) | Some an -> an
        let qast = QuotationCompiler.ToParsedInput(expr)
        let dependencies = qast.Dependencies |> List.map (fun a -> a.Location)
        let sscs = FSharpChecker.Create()
        match sscs.CompileToDynamicAssembly([qast.Tree], assemblyName, dependencies, None, debug = false) |> Async.RunSynchronously with
        | _, _, Some a -> a.GetType(qast.ModuleName).GetMethod(qast.FunctionName)
        | errors, _, _ -> raise <| new QuotationCompilerException (printErrors errors)

    /// <summary>
    ///     Compiles provided quotation tree to function.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="useCache">Keep compiled functions for syntactically equal quotations in cache. Defaults to true.</param>
    static member ToFunc(expr : Expr<'T>, ?useCache:bool) : unit -> 'T =
        let useCache = defaultArg useCache true
        let compile _ =
            let methodInfo = QuotationCompiler.ToDynamicAssembly expr
            if typeof<'T> = typeof<unit> then
                let action = wrapDelegate<Action>(methodInfo)
                fun () -> action.Invoke() ; Unchecked.defaultof<'T>
            else
                let func = wrapDelegate<Func<'T>>(methodInfo)
                func.Invoke

        if useCache then
            compiledExprs.GetOrAdd(expr, compile >> box) :?> unit -> 'T
        else
            compile ()

    /// <summary>
    ///     Compiles provided quotation tree to value.
    /// </summary>
    /// <param name="expr">Quotation tree to be compiled.</param>
    /// <param name="useCache">Keep compiled functions for syntactically equal quotations in cache. Defaults to true.</param>
    static member Eval(expr : Expr<'T>, ?useCache : bool) : 'T =
        QuotationCompiler.ToFunc(expr, ?useCache = useCache) ()