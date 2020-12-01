#r @"nuget: Unquote"
open Swensen.Unquote

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path |> Seq.map int

let combinations list =
    let rec combinations acc list =
        match list with
        |x :: xs -> 
            let withX = xs |> List.map (fun y -> (x,y))
            combinations (withX :: acc) xs
        | _ -> acc |> List.concat
    combinations [] list

let combinations3 list =
    let rec combinations3 acc list =
        match list with
        |x :: xs -> 
            let tuples = combinations xs
            let withX = tuples |> List.map (fun (y,z) -> (x,y,z))
            combinations3 (withX :: acc) xs
        | _ -> acc |> List.concat
    combinations3 [] list

let part2 =
    input
    |> Seq.toList
    |> combinations3
    |> List.find (fun (x,y,z) -> x + y + z = 2020)
    |> (fun (x,y,z) -> x * y * z)

printf "Testing..."
test <@ combinations [1;2;3;4] = [(3, 4); (2, 3); (2, 4); (1, 2); (1, 3); (1, 4)] @>
test <@ combinations3 [1;2;3;4] = [(2, 3, 4); (1, 3, 4); (1, 2, 3); (1, 2, 4)] @>
printfn "done!"
