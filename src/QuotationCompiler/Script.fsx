#I "../../bin/"
#r "FSharp.Compiler.Service.dll"
#r "QuotationCompiler.dll"

open QuotationCompiler

#r "/Users/eirik/Desktop/test.dll"

let f =
    QuotationCompiler.ToFunc 
        <@
            Test.Foo.A 42 : Test.Foo<System.Int32>
        @>

let ast =
    <@ 
        Test.Foo.A 42 : Test.Foo<System.Int32> 
    @>
    |> QuotationCompiler.ToParsedInput
    |> fst


let tree = Ast.ofSourceString """
module Foo
    
let x = Test.Foo.A 42
"""