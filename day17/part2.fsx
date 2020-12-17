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
    let offsets = [-1;0;1]
    [ for x in offsets do
      for y in offsets do
      for z in offsets do
      for w in offsets ->
        (x,y,z,w)]
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
//Real: 00:00:05.422, CPU: 00:00:05.406, GC gen0: 2308, gen1: 29, gen2: 0
let part2 = final |> Map.filter(fun k s -> s = Active) |> Seq.length

printf "Test.."
printfn "..done!"