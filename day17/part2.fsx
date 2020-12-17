#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  

type CubeState = Active | Inactive
type PocketDimension = Map<int*int*int*int, CubeState>
let parse input : PocketDimension =
    let parseState = 
        function 
        | '.' -> Inactive 
        | '#' -> Active 
        | u -> failwith $"unknown input character: {u}"
    [for (row,line) in input |> Seq.indexed do
        for (col,char) in line |> Seq.indexed ->
            (col,row,0,0), parseState char ]
    |> Map.ofList

let neighbours dimension (x,y,z,w) =
    [
        (-1,-1);(0,-1);(1,-1)
        (-1,0); (0,0); (1,0)
        (-1,1); (0,1); (1,1)
    ]
    |> Seq.collect (fun (xo,yo) -> [ (xo,yo,0); (xo,yo,1);(xo,yo,-1) ])
    |> Seq.collect (fun (xo,yo,zo) -> [ (xo,yo,zo,0); (xo,yo,zo,1);(xo,yo,zo,-1) ])
    |> Seq.except [(0,0,0,0)]
    |> Seq.map (fun (xo,yo,zo,wo) -> (x+xo,y+yo,z+zo,w+wo))

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

let numberActiveNeighbours dimension coord =
    neighbours dimension coord 
    |> Seq.map (stateAt dimension) 
    |> Seq.filter (fun s -> s = Active)
    |> Seq.length

let applyRules dimension coord =
    let cubeState = stateAt dimension coord
    let nbActiveNeighbours = numberActiveNeighbours dimension coord
    let nextCubeState =
        match cubeState, nbActiveNeighbours with
        | Active, a when a = 2 || a = 3 -> Active
        | Active, _ -> Inactive
        | Inactive, 3 -> Active
        | Inactive, _ -> Inactive
    coord, nextCubeState

let nextCycle (dimension : PocketDimension) =
    printfn "Calculating a cycle..."
    let candidates = candidates dimension
    let updates = candidates |> Seq.map (applyRules dimension)
    let next = updates |> Seq.fold (fun d (coord,state) -> d |> Map.add coord state) dimension
    next |> Map.filter (fun k v -> v = Active)

let pd = parse input

#time "on"
let final = pd |> nextCycle|> nextCycle|> nextCycle|> nextCycle|> nextCycle|> nextCycle
//Real: 00:00:46.209, CPU: 00:00:45.234, GC gen0: 14524, gen1: 75, gen2: 1
//Real: 00:00:07.055, CPU: 00:00:06.984, GC gen0: 2309, gen1: 35, gen2: 0
let part2 = final |> Map.toSeq |> Seq.map snd |> Seq.filter (fun s -> s = Active) |> Seq.length

printf "Test.."
printfn "..done!"