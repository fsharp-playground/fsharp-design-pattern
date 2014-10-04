module AbserverPatternTest

open System

type Subject() =
    let mutable notify = fun _ -> ()
    member this.Subscribe notifyFunction = 
        let wrap f i = f i; i
        notify <- wrap notifyFunction >> notify

    member this.Reset() = notify <- fun _ -> ()

    member this.SomthingHappen k = 
        notify k

type ObserverA() =
    member this.NotifyMe i = printfn "notified A %A" i

type ObserverB() =
    member this.NotifyMe i = printfn "notified B %A" i 

let observer() =
    let a = ObserverA()
    let b = ObserverB()

    let subject = Subject()
    subject.Subscribe a.NotifyMe
    subject.Subscribe b.NotifyMe

    subject.SomthingHappen "good"


let usingObservableModule() =
    let myEvent = Event<_>()
    let observerA = fun i -> printfn "abserver A noticed something, its value is %A" i
    let observerB = fun i -> printfn "abserver B noticed something, its value is %A" i
    let observerC = fun i -> printfn "abserver C noticed something, its vlaue is %A" i

    myEvent.Publish
    |> Observable.add observerA

    myEvent.Publish
    |> Observable.add observerB

    myEvent.Publish
    |> Observable.add observerC

    myEvent.Trigger 1