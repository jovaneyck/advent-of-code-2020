#r @"nuget: Unquote"
open Swensen.Unquote

let candidates startJoltage adapters = 
    let rec candidates' acc startJoltage adapters =
        match adapters with
        | [] -> acc
        | a :: adapts when a - startJoltage <= 3 -> (candidates' ((a, adapts) ::acc) startJoltage adapts)
        | _ :: adapts -> acc
    candidates' [] startJoltage adapters |> List.rev

let rec calculateArrangements startJoltage adapters : int =
    match adapters with
    | [] -> 1
    | [_] -> 1
    | adapters ->
        //printfn "calculate"
        //printfn $"Joltage: {startJoltage}"
        //printfn $"Adapters: {adapters}"
        let candidates = candidates startJoltage adapters
        //printfn $"Candidates:"
        //candidates |> Seq.iter (printfn "%A")

        let numberPaths = candidates |> Seq.length
        //printfn $"numberPaths: {numberPaths}"

        candidates 
        |> Seq.map (fun (a,aas) -> calculateArrangements a aas) 
        //|> (fun x-> printfn "recursive current joltage %A: %A" startJoltage (x |> Seq.toList);  x)
        |> Seq.sum
   

let solve adapters =
    let sorted = adapters |> List.sort
    let deviceAdapter = 3 + (sorted |> Seq.last)
    let includingDevice = sorted @ [deviceAdapter]

    calculateArrangements 0 includingDevice

let smallExample = [1; 4; 5; 6; 7; 10; 11; 12; 15; 16; 19]
let largerExample = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\larger-example.txt" |> Seq.map int |> Seq.toList
printf "Test.."
test <@ solve smallExample = 8 @> 
test <@ solve largerExample = 19208 @> 
printfn "done!"

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int
