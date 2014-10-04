module StrategyPatternTest

open System

let quicksort l =
    printfn "quick sort"

let sellsort l =
    printfn "shell sort"

let bubblesort l =
    printfn "bubble sort"

type Strategy() =
    let mutable sortFunction = fun _ -> ()
    member this.SetStrategy f = sortFunction <- f
    member this.Execute n = sortFunction n

let strategy() =
    let s = Strategy()
    s.SetStrategy quicksort
    s.Execute [1..6]

    s.SetStrategy bubblesort
    s.Execute [1..6]

let usingHigherOrderFunction() = 
    let executeStrategy f n = f n
    let s = executeStrategy quicksort

    [1..6] |> s

    let s2 = executeStrategy bubblesort
    [1..6] |> s2
\oopenopesfsssopนยำืหหหหหsslslsfslfjslfjs