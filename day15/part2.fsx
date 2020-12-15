#r @"nuget: Unquote"
open Swensen.Unquote

let input = 
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"  
    |> (fun s -> s.Split([|","|], System.StringSplitOptions.None))
    |> Seq.map int64
    |> Seq.toList
let example = [0L;3L;6L]

type Turn = { timestamp : int64; number : int64; lookup : Map<int64, int64> (*nb->last time we heard nb*) }
let initialTurn seed =
    let lookup = seed |> Seq.mapi (fun idx nb -> (nb, int64 idx)) |> Map.ofSeq
    { timestamp = (seed |> Seq.length |> int64) - 1L; number = (seed |> Seq.last); lookup = lookup}

let next turn =
    let nextTime = turn.timestamp + 1L
    match turn.lookup |> Map.tryFind turn.number with
    | None ->
        { turn with 
            timestamp = nextTime
            number = 0L
            lookup = turn.lookup |> Map.add turn.number turn.timestamp }
    | Some t when t = turn.timestamp ->
        { turn with 
            timestamp = nextTime
            number = 0L
            lookup = turn.lookup |> Map.add turn.number turn.timestamp }
    | Some t -> 
        let nextNumber = turn.timestamp - t
        { turn with
            timestamp = nextTime
            number = nextNumber
            lookup = turn.lookup |> Map.add turn.number turn.timestamp }

let rec generateSeq turn : Turn seq =
    seq {
        yield turn
        if turn.timestamp % 300_000L = 0L then printfn $"{turn.timestamp}"
        yield! generateSeq (next turn)
    }


let solve input n =
    let init = initialTurn input
    let nth = generateSeq init |> Seq.map (fun t -> t.number) |> Seq.item (n - (input |> Seq.length))
    nth
    
let part1 = solve input 2020 //2020: 1238L
#time "on" 
let part2 = solve input 30_000_000
(*
    Real: 00:01:24.886, CPU: 00:01:24.375, GC gen0: 5599, gen1: 857, gen2: 93
    val part2 : int64 = 3745954L
*)

printf "Test"
printfn "done!"