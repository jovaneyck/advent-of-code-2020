﻿#r @"nuget: Unquote"
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

let rec findFirstSeatIn layout (x,y) (dx, dy) =
    let next = (x+dx, y+dy)
    match layout |> Map.tryFind next with
    | None -> None
    | Some Floor -> findFirstSeatIn layout next (dx,dy)
    | Some seat -> Some seat

let neighbours layout location =
    let deltas =
        [(-1,-1);(0,-1);(1,-1)
         (-1,0);        (1,0);
         (-1,1); (0,1); (1,1)]
    deltas |> Seq.choose (findFirstSeatIn layout location)

let applyRules layout (coord, s) =
    let neighb = neighbours layout coord 
    let occupied = neighb |> Seq.filter (function | Occupied -> true | _ -> false) |> Seq.length
    let next =
        match s, occupied with
        | EmptySeat, 0 -> Occupied
        | Occupied, nb when nb >= 5 -> EmptySeat
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
#time
let final = fixp tick layout //Real: 00:00:31.948, CPU: 00:00:26.859, GC gen0: 5049, gen1: 10, gen2: 1
final |> Map.toSeq |> Seq.map snd |> Seq.filter (function | Occupied -> true | _ -> false) |> Seq.length 

let ex1 = [ "............."
            ".L.L.#.#.#.#."
            "............."]
printf "Test.."
test <@ neighbours (parse ex1) (1,1) |> Seq.toList = [EmptySeat]  @>
test <@ neighbours (parse ex1) (3,1) |> Seq.toList = [EmptySeat; Occupied]  @>
test <@ 1+1=2 @> 
printfn "done!"