#r @"nuget: Unquote"
open Swensen.Unquote

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path |> Seq.map int

let rec combinations list =
    match list with
    |x :: xs -> 
        let withX = xs |> List.map (fun y -> (x,y))
        let others = combinations xs
        List.append withX others
    | _ -> []

let part1 =
    input
    |> Seq.toList
    |> combinations
    |> List.find (fun (x,y) -> x + y = 2020)
    |> (fun (x,y) -> x * y)
printf "Testing..."
test <@ combinations [1;2;3;4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)] @>
printfn "done!"
