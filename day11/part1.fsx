#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"

type State = Floor | EmptySeat | Occupied

let parse text =
    let parseChar =
        function
        | '.' -> Floor
        | 'L' -> EmptySeat
        | '#' -> Occupied
        | e -> failwith $"Unknown character: {e}"

    [ for (y,row) in text |> Seq.indexed do
        for (x,col) in row |> Seq.indexed ->
            (x,y), parseChar col ]
    |> Map.ofSeq

let neighbours layout (cx,cy) =
    let deltas =
        [(-1,-1);(0,-1);(1,-1)
         (-1,0);        (1,0);
         (-1,1); (0,1); (1,1)]
    let coords = deltas |> Seq.map (fun (x,y) -> cx + x, cy + y )
    coords |> Seq.choose (fun c -> layout |> Map.tryFind c)

let applyRules layout (coord, s) =
    let neighb = neighbours layout coord 
    let occupied = neighb |> Seq.filter (function | Occupied -> true | _ -> false) |> Seq.length
    let next =
        match s, occupied with
        | EmptySeat, 0 -> Occupied
        | Occupied, nb when nb >= 4 -> EmptySeat
        | s, _ -> s
    (coord, next)

let tick layout =
    layout 
    |> Map.toSeq
    |> Seq.map (applyRules layout)
    |> Map.ofSeq

let rec fixp f x =
    let next = f x
    if next = x
    then next
    else fixp f next

let layout = parse input
let final = fixp tick layout
final |> Map.toSeq |> Seq.map snd |> Seq.filter (function | Occupied -> true | _ -> false) |> Seq.length 
//tick layout |> Seq.iter (printfn "%A")

printf "Test.."
test <@ 1+1=2 @> 
printfn "done!"