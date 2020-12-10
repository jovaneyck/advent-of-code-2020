#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int |> Seq.toList
let example = [16;10;15;5;1;11;7;19;6;12;4]

let candidates joltage adapters =
    let rec candidates' acc joltage adapters =
        match adapters with
        | [] -> acc
        | a :: aas when a - joltage <= 3 ->  candidates' ((a, aas) ::acc) joltage aas
        | _ -> acc
    candidates' [] joltage adapters |> List.rev

let rec calculateNbPaths memo joltage adapters = 
    match memo |> Map.tryFind joltage with
    | Some v -> memo
    | None ->
        match adapters with
        | [_] -> 
            memo |> Map.add joltage 1L
        | adapters ->
            let cands = candidates joltage adapters 
            let nextMemo = cands |> Seq.fold (fun memo (a, aas) -> calculateNbPaths memo a aas) memo
            let sum = cands |> Seq.map fst |> Seq.map (fun c -> nextMemo |> Map.find c) |> Seq.sum
            nextMemo |> Map.add joltage sum

let solve adapters =
    let sorted = adapters |> List.sort
    let deviceAdapter = (sorted |> Seq.last) + 3
    let allAdapters = sorted @ [deviceAdapter]
    let memo = Map.empty
    
    calculateNbPaths memo 0 allAdapters |> Map.find 0

printf "Test.."
test <@ candidates 4 [5;6;7;10] = [(5, [6; 7; 10]); (6, [7; 10]); (7, [10])] @>
test <@ solve example = 8L @> 
test <@ solve input = 99214346656768L @> 
printfn "done!"