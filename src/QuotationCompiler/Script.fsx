#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

let f =
    QuotationCompiler.ToFunc
        <@ 
            match Choice1Of2 12 with 
            | Choice1Of2 i -> i
            | _ -> 0
        @>

let ast =
    <@ 
        match Choice1Of2 12 with 
        | Choice1Of2 i -> i
        | _ -> 0
    @>
    |> QuotationCompiler.ToParsedInput 
    |> snd

f ()


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

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape

let rec tryGetGetter (expr : Expr) =
    match expr with
    | PropertyGet(_,p,_) -> Some p
    | ShapeVar _ -> None
    | ShapeLambda(_,b) -> tryGetGetter b
    | ShapeCombination(_,exprs) -> List.tryPick tryGetGetter exprs

let item =
    tryGetGetter 
        <@ 
            match Choice1Of2 12 with 
            | Choice1Of2 i -> i
            | _ -> 0
        @>
    |> Option.get

item.DeclaringType