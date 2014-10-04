module DesignPatternTest

open System

let usingRefKeyword() =
    let a = ref 0
    let increaseA () =
        a := !a + 1

    increaseA()
    printfn "a = %A" !a

type Command = { Redo: unit -> unit; Undo: unit -> unit }

let commandPattern() =
    let result = ref 7
    let add n = 
        {
            Redo = (fun _ -> result := !result + n)
            Undo = (fun _ -> result := !result - n)
        }

    let minus n = 
        { 
            Redo = (fun _ -> result := !result - n)
            Undo = (fun _ -> result := !result + n)
        }

    let cmd = add 3
    printfn "current state = %d" !result

    cmd.Undo()
    printfn "current state = %d" !result

    cmd.Redo()
    printfn "current state = %d" !result


type DayOfAWeek =
    | Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday

type TWorkingHoure = 
    | Hour of DayOfAWeek * int

let commandUsingDiscriminated() =
    let isWorkingHour day =
        match day with
        | Hour(Sunday, _) -> false
        | Hour(Saturday, _) -> false
        | Hour(_, time) -> time >= 9 && time <= 17

    let sunday = Hour(Sunday, 9)
    printfn "%A is working hour? %A" sunday (isWorkingHour sunday)

    let monday = Hour(Monday, 10)
    printfn "%A is working hour? %A" monday (isWorkingHour monday)


type CommandType =
    | Deposit
    | Withdraw

type TCommand =
    | Command of CommandType * int

let commandPatternImpl2() =
    let result = ref 7
    let deposit x = result := !result - x
    let withdraw x = result := !result + x

    let Do = fun cmd ->
        match cmd with
        | Command(Deposit, n) -> deposit n
        | Command(Withdraw, n) -> withdraw n

    let Undo = fun cmd ->
        match cmd with
        | Command(Deposit, n) -> withdraw n
        | Command(Withdraw, n) -> deposit n

    printfn "current balance %d" !result

    let depositCmd = Command(Deposit, 3)
    Do depositCmd
    printfn "after deposit: %d" !result

    Undo depositCmd
    printfn "after undo: %d" !result


