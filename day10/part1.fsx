#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int
let sorted = input |> Seq.sort |> Seq.toList
let last = sorted |> Seq.last
let totalChain = (0 :: sorted) @ [last + 3]

let deltas = totalChain |> List.pairwise |> List.map (fun (a,b) -> b-a) |> List.groupBy id
let ones = (snd deltas.[0]) |> Seq.length
let threes = (snd deltas.[1]) |> Seq.length
ones * threes
printf "Test.."
test <@ 1+1=2 @> 
printfn "done!"