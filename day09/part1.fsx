#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
let parse : string seq -> int64 seq = Seq.map int64

let windowSize = 25

let parsed = parse input

let pairs (candidates : 'a seq) =
    let indexed = candidates |> Seq.indexed |> Seq.toList
    [for (i,a) in indexed do
        for (_,b) in indexed.[i+1..]  -> 
            (a,b)]

let problematicNumber window = 
    let number = window |> Seq.last
    let candidates = window |> Seq.rev |> Seq.skip 1
    let sums = candidates |> pairs |> Seq.map (fun (a,b) -> a+b)
    sums |> Seq.contains number |> not

let part1 = 
    parsed 
    |> Seq.windowed (windowSize + 1)
    |> Seq.find problematicNumber


printf "Test.."
test <@ pairs [1..3] = [(1, 2); (1, 3); (2, 3)] @> 
printfn "done!"