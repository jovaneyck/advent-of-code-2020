#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int
let sorted = input |> Seq.sort |> Seq.toList

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
   
let smallExample = [1; 4; 5; 6; 7; 10; 11; 12; 15; 16; 19]

calculateArrangements 0 (smallExample @ [22])
calculateArrangements 0 [1; 2; 4;]

let deviceAdapter = 3 + (sorted |> Seq.last)
let includingDevice = sorted @ [deviceAdapter]

calculateArrangements 0 includingDevice

printf "Test.."
test <@ candidates 0 [1..4] = [(1, [2; 3; 4]); (2, [3; 4]); (3, [4])] @> 
test <@ candidates 2 [3..4] = [(3, [4]); (4, [])] @> 
printfn "done!"