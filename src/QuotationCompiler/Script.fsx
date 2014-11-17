#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let f =
    QuotationCompiler.ToFunc
        <@ match Choice<int,int>.Choice1Of2 12 with Choice2Of2 _ -> 0 | Choice1Of2 i -> i @>

let ast =
    <@
        match None : int option with 
        | Some _ -> true
        | _ -> false
    @>
    |> QuotationCompiler.ToParsedInput 
    |> snd

f ()


let tree =
    Ast.ofSourceString """
    module Foo
    
    let x = match None with None -> true | _ -> false
    """