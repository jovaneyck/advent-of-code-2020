#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  

type CubeState = Active | Inactive
type PocketDimension = Map<int*int*int, CubeState>
let parse input : PocketDimension =
    let parseState = function | '.' -> Inactive | '#' -> Active | u -> failwith $"unknown input character: {u}"
    [for (row,line) in input |> Seq.indexed do
        for (col,char) in line |> Seq.indexed ->
            (col,row,0), parseState char ]
    |> Map.ofList

let neighbours dimension (x,y,z) =
    [
        (-1,-1);(0,-1);(1,-1)
        (-1,0); (0,0); (1,0)
        (-1,1); (0,1); (1,1)
    ]
    |> Seq.collect (fun (xo,yo) -> [ (xo,yo,0); (xo,yo,1);(xo,yo,-1) ])
    |> Seq.except [(0,0,0)]
    |> Seq.map (fun (xo,yo,zo) -> (x+xo,y+yo,z+zo))

let candidates dimension = 
    let candidates = dimension |> Map.toSeq |> Seq.map fst
    
    candidates 
    |> Seq.collect (neighbours dimension) 
    |> Seq.append candidates
    |> Seq.distinct

let stateAt dimension coord =
    dimension 
    |> Map.tryFind coord
    |> Option.defaultValue Inactive

let applyRules dimension coord =
    let cubeState = stateAt dimension coord
    let nbActiveNeighbours = 
        neighbours dimension coord 
        |> Seq.map (stateAt dimension) 
        |> Seq.filter (fun s -> s = Active)
        |> Seq.length
    let nextCubeState =
        match cubeState, nbActiveNeighbours with
        | Active, a when a = 2 || a = 3 -> Active
        | Active, _ -> Inactive
        | Inactive, 3 -> Active
        | Inactive, _ -> Inactive
    coord, nextCubeState

let nextCycle (dimension : PocketDimension) =
    let candidates = candidates dimension
    let updates = candidates |> Seq.map (applyRules dimension)
    updates |> Seq.fold (fun d (coord,state) -> d |> Map.add coord state) dimension

let pd = parse input
let final = pd |> nextCycle|> nextCycle|> nextCycle|> nextCycle|> nextCycle|> nextCycle
let part1 = final |> Map.toSeq |> Seq.map snd |> Seq.filter (fun s -> s = Active) |> Seq.length

printf "Test.."
printfn "..done!"