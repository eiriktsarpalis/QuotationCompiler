module Foobar

let (|Zero|Positive|Negative|) (x : int) =
    if x = 0 then Zero
    elif x < 0 then Negative x
    else Positive (x, x % 2 = 0)

let test n =
    match n with
    | Zero -> printfn "zero"
    | Positive(n, isEven) -> printfn "positive %d (even %b)" n isEven
    | Negative n -> printfn "negative %d" n

let inline classify zKont pKont nKont x =
    if x = 0 then zKont ()
    elif x < 0 then nKont x
    else pKont x (x % 2 = 0)

let test' x n =
    classify 
        (fun () -> printfn "zero %b" x) 
        (fun n isEven -> printfn "positive %d (even %b)" n isEven)
        (fun n -> printfn "negative %d" n)
        n


let test0 n =
    match n with
    | -1 -> printfn "negative 1"
    | Zero -> printfn "zero"
    | Positive(n, isEven) -> printfn "positive %d (even %b)" n isEven
    | -5 -> printfn "negative 5"
    | Negative n -> printfn "negative %d" n

let test0' n =
    match n with
    | -1 -> printfn "negative 1"
    | _ ->
        classify
            (fun () -> printfn "zero")
            (fun n isEven -> printfn "positive %d (even %b)" n isEven)
            (fun x -> 
                match n with
                | -5 -> printfn "negative 5"
                | _ -> printfn "negative %d" x)
            n