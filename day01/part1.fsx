#r @"nuget: Unquote"
open Swensen.Unquote

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path |> Seq.map int

let combinations list =
    let indexed = list |> List.indexed
    [ for (i,x) in indexed do
      for (j,y) in indexed.[i+1..] -> 
        (x,y)]

let combinations3 list =
    let indexed = list |> List.indexed
    [ for (i,x) in indexed do
      for (j,y) in indexed.[i+1..] do 
      for (k,z) in indexed.[j+1..] -> 
        (x,y,z)]

input 
|> Seq.toList 
|> combinations3
|> List.find (fun (x,y,z) -> x + y + z = 2020)
|> (fun (x,y,z) -> x * y * z)

printf "Testing..."
test <@ combinations [1;2;3;4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)] @>
test <@ combinations3 [1;2;3;4] = [(1, 2, 3); (1, 2, 4); (1, 3, 4); (2, 3, 4)]  @>
printfn "done!"
