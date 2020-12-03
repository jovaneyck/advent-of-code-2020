#r @"nuget: Unquote"
open Swensen.Unquote

let rec combinationsRecursive list =
    match list with
    |x :: xs -> 
        let withX = xs |> List.map (fun y -> (x,y))
        let others = combinationsRecursive xs
        List.append withX others
    | _ -> []

let combinationsComprehensions list =
    let indexed = list |> List.indexed
    [ for (i,x) in indexed do
      for (j,y) in indexed.[i+1..] -> 
        (x,y)]

test <@ combinationsRecursive [1..4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)]  @>
test <@ combinationsRecursive [1..4] = combinationsComprehensions [1..4] @>

test <@ combinationsRecursive [1;2;1;4] = [(1, 2); (1, 1); (1, 4); (2, 1); (2, 4); (1, 4)]  @>
test <@ combinationsRecursive [1;2;1;4] = combinationsComprehensions [1;2;1;4] @>