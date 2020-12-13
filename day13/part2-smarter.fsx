#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let parse (text : string) =
    let busids = 
        text 
        |> fun s -> s.Split([|','|]) 
        |> Seq.map (fun n -> if n = "x" then 1L else int64 n)
        |> Seq.toList
    busids

let solveMutable (busids : int64 list list) (timestamp : int64) increment =
    let mutable ts = timestamp
    let mutable searching = true
    let nbBuses = busids |> Seq.length
    while searching do
        //if ts % 1_000_000L = 0L
        //then printfn $"{ts}"

        let mutable intidx = 0
        let mutable idx = 0L
        let mutable problem = false
        while not problem && intidx < nbBuses do
            let busid = busids.[intidx].[0]
            let rem = busids.[intidx].[1]
            if ts % busid <> rem then
                problem <- true
            else
                intidx <- intidx + 1
                idx <- idx + 1L

        if not problem 
        then searching <- false 
        else ts <- ts + increment
    ts

let findSeed seed (maxIdx, maxID) =
    printfn "finding seed for %A" (seed ,maxIdx, maxID)
    let multiples nb = Seq.initInfinite (fun idx -> int64 idx * nb)
    let start = maxID - int64 maxIdx
    let ms = multiples maxID |> Seq.map (fun m -> m + start)
    let smallestValidSeed = ms |> Seq.find (fun m -> m > seed)
    printfn "Found seed, let's gooooo! %A" smallestValidSeed
    smallestValidSeed

let solve seed input =
    let busIDs = parse input
    let withRem = busIDs |> Seq.mapi (fun i busid -> [busid; if i = 0 then 0L else if busid = 1L then 0L else busid - int64 i]) |> Seq.toList
    let (maxIdx,maxID) = busIDs |> Seq.indexed |> Seq.maxBy snd
    let seed = findSeed seed (maxIdx, maxID)
    solveMutable withRem seed maxID

printf "Test.."
//#time "on"
test <@ solve 0L "17,x,13,19" = 3_417L @>
test <@ solve 0L "67,7,59,61" = 754_018L @>
test <@ solve 0L "67,x,7,59,61" = 779210L @>
test <@ solve 0L "67,7,x,59,61" = 1261476L @>
test <@ solve 0L "1789,37,47,1889" = 1_202_161_486L @> 
printfn "done!"

findSeed 100000000000000L, (19, 743L)

let rec alert () =
    System.Console.Beep(500,1000)
    System.Console.Beep(1000,1000)
    alert ()

let FINALLY = solve 100000000000000L (input |> Seq.item 1)
printfn $"{FINALLY}"
alert () |> ignore
