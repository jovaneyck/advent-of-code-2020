#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let parse (text : string) =
    let busids = 
        text 
        |> fun s -> s.Split([|','|]) 
        |> Seq.map (fun n -> if n = "x" then None else Some <| int64 n)
        |> Seq.indexed
        |> Seq.filter (function | (_, Some _) -> true | _ -> false)
        |> Seq.map (function | (idx, Some ts) -> (idx, ts) | _ -> (0,0L))
        |> Seq.toList
    busids

let arrivalsFor busID =
    Seq.initInfinite (fun idx -> (idx |> int64) * busID) |> Seq.skip 1

let ticks =
    let rec ticks ts = 
        seq {
            yield ts
            yield! ticks (ts + 1L)
        }
    ticks 0L

let arrivalTimes parsedInput = parsedInput |> Seq.map (fun (offset,busid) -> (offset, arrivalsFor busid))

type State = { timestamp : int64; foundSolution : bool; arrivalTimes : (int * int64 seq) seq}
let init times = { timestamp = 0L; foundSolution = false; arrivalTimes = times }

let applies (state : State) (ts :int64) = 
    //printfn $"{ts}"
    let arrivalTimesInFuture =
        state.arrivalTimes
        |> Seq.map (fun (offset, arrivals) -> 
            let futureArrivals = arrivals |> Seq.skipWhile (fun n -> n < ts) 
            (offset, futureArrivals))
        
    let foundSolution =
        arrivalTimesInFuture
        |> Seq.forall (fun (offset : int, times) -> 
                            times 
                            |> Seq.head
                            |> (fun h -> h = (ts + (int64 offset))))
    { state with timestamp = ts; foundSolution = foundSolution }

let solve input =
    let parsed = input |> parse
    ticks 
    |> Seq.scan applies (init (arrivalTimes parsed))
    |> Seq.find (fun s -> s.foundSolution)
    |> (fun s -> s.timestamp)

let busids = input |> Seq.item 1 |> parse

printf "Test.."
#time "on"
test <@ solve  "17,x,13,19" = 3_417L @> //Real: 00:00:00.185, CPU: 00:00:00.171, GC gen0: 39, gen1: 0, gen2: 0
//test <@ solve  "67,7,59,61" = 754_018L @> //Real: 00:14:28.726, CPU: 00:14:24.437, GC gen0: 483935, gen1: 87, gen2: 8
printfn "done!"