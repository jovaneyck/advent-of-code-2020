#r @"nuget: Unquote"
open Swensen.Unquote
open System.Collections.Generic

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int |> Seq.toList
let example = [16;10;15;5;1;11;7;19;6;12;4]

let candidates joltage adapters =
    let rec candidates' acc joltage adapters =
        match adapters with
        | [] -> acc
        | a :: aas when a - joltage <= 3 ->  candidates' ((a, aas) ::acc) joltage aas
        | _ -> acc
    candidates' [] joltage adapters |> List.rev

let rec calculateNbPaths (memo : Dictionary<int,int64>) joltage adapters = 
    match memo.TryGetValue joltage with
    | (true, v) -> v
    | _ ->
        match adapters with
        | [_] -> 
            memo.Add(joltage, 1L)
            1L
        | adapters ->
            let cands = candidates joltage adapters 
            let sum = cands |> Seq.map (fun (a, aas) -> calculateNbPaths memo a aas) |> Seq.sum
            memo.Add(joltage, sum)
            sum

let solve adapters =
    let sorted = adapters |> List.sort
    let deviceAdapter = (sorted |> Seq.last) + 3
    let allAdapters = sorted @ [deviceAdapter]
    let memo = Dictionary<int,int64> ()
    
    calculateNbPaths memo 0 allAdapters

printf "Test.."
test <@ candidates 4 [5;6;7;10] = [(5, [6; 7; 10]); (6, [7; 10]); (7, [10])] @>
test <@ solve example = 8L @> 
test <@ solve input = 99214346656768L @> 
printfn "done!"