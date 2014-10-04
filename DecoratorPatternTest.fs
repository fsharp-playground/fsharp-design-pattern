module DecoratorPatternTest

open System

type Devider() =
    let mutable devide = fun (a,b) -> a / b

    member this.Function 
        with get() = devide
        and set(v) = devide <- v

    member this.Invoke(a,b) = devide (a,b)

let decorate() =

    let d = Devider()
    let checkZero(a,b) = if b = 0 then failwith "a/b and b is o" else (a,b)

    try 
        d.Invoke(1, 0) |> ignore
    with e -> printfn "without check, the error is =  %s" e.Message

    d.Function <- checkZero >> d.Function

    try 
        d.Invoke(1,0) |> ignore
    with e -> printfn "after add check, error is %s" e.Message
  
