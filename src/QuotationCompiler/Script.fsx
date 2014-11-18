#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let f =
    QuotationCompiler.ToFunc 
        <@
            match [2] with
            | i :: [] when i = 2 -> true
            | [] -> false
            | _ -> false
        @>

<@ async @>

let ast =
    <@ 
        match [2] with
        | 2 :: [] -> true
        | _ -> false
    @>
    |> QuotationCompiler.ToParsedInput 
    |> snd


let tree = Ast.ofSourceString """
module Foo
    
let x =         
    match y with 
    | Bar(name = x; age = 2) -> x
"""

type Foo =
    | Bar of name:string * age:int

<@ 
    match Bar("me",29) with 
    | Bar(name = n; age = 2) -> n
@>

<@ 
    match Choice1Of2 12 with 
    | Choice1Of2 1 -> 1
    | Choice2Of2 2 -> 2
    | _ -> 0
@>