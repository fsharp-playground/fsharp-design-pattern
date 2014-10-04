module ChainOfResponsibilityPatternTest

type Record = 
    {
        Name: string
        Age: int
        Weight: float
        Height: float
    }

let chainOfResponsibilty() =

    let validAge record =
        record.Age < 65 && record.Age > 18

    let validWeight record = 
        record.Weight < 200.

    let validHeight record =
        record.Height > 120.

    let check f (record, result) =
        if not result then record, false
        else record, f(record)

    let chainOfResponsibility = check validAge >> check validWeight >> check validHeight
    let john = { Name= "John"; Age = 80; Weight = 180.; Height = 180. }
    let dan = { Name= "Dan"; Age = 20; Weight= 160.; Height=190. }

    printfn "John's result = %b" (chainOfResponsibility (john, true) |> snd)
    printfn "Dan's result = %b" (chainOfResponsibility (dan, true) |> snd)


let chainUsingPipeline() =
    let chainTemplate processFunction canContinue s = 
        if canContinue s then
            processFunction s
        else s

    let canContinueF it = 
        if it < 2 then true else false

    let processF x = x + 1
    let chainFunction = chainTemplate processF canContinueF
    let s = 1 |> chainFunction |> chainFunction

    printfn "%A" s


[<Measure>] type kg

let chainUsingPartial() = 
    let oneKilo = 1<kg>
    let twoKilo = 1<kg> + 1<kg>

    let (| Odd | _ |) x = if x % 2 = 0 then None else Some(x)
    let findOdd x = 
        match x with
        | Odd x -> printfn "x is odd number"
        | _ -> printfn "x is not odd number"

    findOdd 3
    findOdd 4


[<Measure>] type cm

type Person() =
    member val Height = 0.<cm> with get, set
    member val Weight = 0.<kg> with get, set

let chainUsingPartialMatching() =
    let makeCheck passingCriterion (person: #Person) =
        if passingCriterion person then None
        else Some(person)
       
    let (| NotPassHeight | _ |) person = makeCheck (fun p -> p.Height > 170.<cm>) person
    let (| NotPassWeight | _ |) person = 
        makeCheck (fun p -> p.Weight < 100.<kg> && p.Weight > 50.<kg>) person

    let check x = 
        match x with
        | NotPassHeight x -> printfn "this person is not tall enough"
        | NotPassWeight x -> printfn "this person is out of weight range"
        | _ -> printfn "good, this person passes"

    let person = Person(Height = 180.<cm>, Weight = 75.<kg>)
    check person