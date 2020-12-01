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

let rec combinations3 list =
    match list with
    |x :: xs -> 
        let tuples = combinations xs
        let withX = tuples |> List.map (fun (y,z) -> (x,y,z))
        let others = combinations3 xs
        List.append withX others
    | _ -> []

let part2 =
    input
    |> Seq.toList
    |> combinations3
    |> List.find (fun (x,y,z) -> x + y + z = 2020)
    |> (fun (x,y,z) -> x * y * z)

printf "Testing..."
test <@ combinations [1;2;3;4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)] @>
test <@ combinations3 [1;2;3;4;5] = [(1, 2, 3); (1, 2, 4); (1, 2, 5); (1, 3, 4); (1, 3, 5); (1, 4, 5); (2, 3, 4);(2, 3, 5); (2, 4, 5); (3, 4, 5)] @>
printfn "done!"
