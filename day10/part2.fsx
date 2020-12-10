#r @"nuget: Unquote"
open Swensen.Unquote

let candidates startJoltage adapters = 
    let rec candidates' acc startJoltage adapters =
        match adapters with
        | [] -> acc
        | a :: adapts when startJoltage - a <= 3L -> (candidates' ((a, adapts) ::acc) startJoltage adapts)
        | _ :: adapts -> acc
    candidates' [] startJoltage adapters |> List.rev

open System.Collections.Generic

let rec calculateArrangements (memo : Dictionary<int64,int64>) startJoltage adapters : int64 =
    let addOrUpdate (memo : Dictionary<int64,int64>) key value =
        match memo.TryGetValue key with
        | (true,_) -> memo.[key] <- value
        | _ -> memo.Add(key,value)
    match memo.TryGetValue startJoltage with
    | (true, value) -> value
    | _ ->
        match adapters with
        | [] -> 1L
        | [_] -> 1L
        | adapters ->
            let candidates = candidates startJoltage adapters |> List.sortByDescending fst
            let sum =
                candidates 
                |> Seq.map (fun (a,aas) -> a, calculateArrangements memo a aas)
                |> Seq.map (fun (a,r ) -> addOrUpdate memo a r; r)
                |> Seq.sum
            memo.Add(startJoltage, sum)
            sum

let solve adapters =
    let sorted = adapters |> List.sort |> List.rev
    let deviceAdapter = 3L + (sorted |> Seq.head)
    let includingDevice = sorted @ [0L]
    let memo = Dictionary<int64,int64>();
    
    calculateArrangements memo deviceAdapter includingDevice

let smallExample = [1L; 4L; 5L; 6L; 7L; 10L; 11L; 12L; 15L; 16L; 19L]
let largerExample = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\larger-example.txt" |> Seq.map int64 |> Seq.toList
printf "Test.."
test <@ solve smallExample = 8L @> 
test <@ solve largerExample = 19208L @> 
printfn "done!"

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int64 |> Seq.toList
#time
solve input
