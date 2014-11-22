#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let f =
    QuotationCompiler.ToFunc 
        <@
            null : string
        @>

let ast =
    <@ 
        match Choice<int,int>.Choice1Of2 12 with 
        | Choice1Of2 i -> i 
        | _ -> -1 
    @>
    |> QuotationCompiler.ToParsedInput 
    |> snd


let tree = Ast.ofSourceString """
module Foo
    
let x = "" = null
"""