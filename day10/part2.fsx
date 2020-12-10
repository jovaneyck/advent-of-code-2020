#r @"nuget: Unquote"
open Swensen.Unquote

let candidates startJoltage adapters = 
    let rec candidates' acc startJoltage adapters =
        match adapters with
        | [] -> acc
        | a :: adapts when a - startJoltage <= 3L -> (candidates' ((a, adapts) ::acc) startJoltage adapts)
        | _ :: _ -> acc
    candidates' [] startJoltage adapters |> List.rev

let rec calculateArrangements (memo : Map<int64,int64>) joltage adapters =       
    match memo |> Map.tryFind joltage with
    | Some _ -> memo
    | _ ->
        match adapters with
        | [] 
        | [_] -> memo |> Map.add joltage 1L
        | adapters ->
            let candidates = candidates joltage adapters
            let newMemo = candidates |> Seq.fold (fun memo (a,aas) -> calculateArrangements memo a aas) memo
            let sum = candidates |> Seq.map fst |> Seq.map (fun c -> newMemo |> Map.find c) |> Seq.sum
            newMemo |> Map.add joltage sum

let solve adapters =
    let sorted = adapters |> List.sort
    let deviceAdapter = 3L + (sorted |> Seq.last)
    let includingDevice = sorted @ [deviceAdapter]
    
    calculateArrangements Map.empty 0L includingDevice |> Map.find 0L

let smallExample = [1L; 4L; 5L; 6L; 7L; 10L; 11L; 12L; 15L; 16L; 19L]
let largerExample = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\larger-example.txt" |> Seq.map int64 |> Seq.toList
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int64 |> Seq.toList

#time
printf "Test.."
test <@ solve smallExample = 8L @> 
test <@ solve largerExample = 19208L @> 
test <@ solve input = 99214346656768L @>
printfn "done!"