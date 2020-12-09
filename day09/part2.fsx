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

let isInvalidNumber window = 
    let number = window |> Seq.last
    let candidates = window |> Seq.rev |> Seq.skip 1
    let sums = candidates |> pairs |> Seq.map (fun (a,b) -> a+b)
    sums |> Seq.contains number |> not

let part1 = 
    parsed 
    |> Seq.windowed (windowSize + 1)
    |> Seq.find isInvalidNumber
    |> Seq.last

let sumsTo number list =
    list |> Seq.sum = number

let tryWindow invalidNumber numbers windowSize =
    numbers |> Seq.windowed windowSize |> Seq.tryFind (sumsTo invalidNumber)

let part2Window =
    [2..1000]
    |> Seq.pick (tryWindow part1 parsed)
let smallest = part2Window |> Seq.min
let largest = part2Window |> Seq.max
let part2 = smallest + largest
printf "Test.."
test <@ pairs [1..3] = [(1, 2); (1, 3); (2, 3)] @> 
printfn "done!"