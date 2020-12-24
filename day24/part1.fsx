#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Direction = NE | E | SE | SW | W | NW
type Color = White | Black

let parsePath (path : string) =
    let rec parsePath (path : char list) =
        match path with
        | [] -> []
        | 'n' :: 'e' :: ps -> NE :: parsePath ps
        | 's' :: 'e' :: ps -> SE :: parsePath ps
        | 'n' :: 'w' :: ps -> NW :: parsePath ps
        | 's' :: 'w' :: ps -> SW :: parsePath ps
        | 'e' :: ps -> E :: parsePath ps
        | 'w' :: ps -> W :: parsePath ps
    parsePath (path |> Seq.toList)

let move (x,y,z) direction =
    let (dx,dy,dz) =
        match direction with
        | E -> (1,1,0)
        | SE -> (1,0,-1)
        | SW -> (0,-1,-1)
        | W -> (-1,-1,0)
        | NW -> (-1,0,1)
        | NE -> (0,1,1)
    (x+dx,y+dy,z+dz)

let rec follow start path =
    match path with
    | [] -> start
    | d :: ds -> follow (move start d) ds

//We're using 3D coordinates to represent locations in the hexagonal grid
//https://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MARTIN/Hex.pdf
type State = Map<(int * int * int), Color>

let flip location state =
    match state |> Map.tryFind location with
    | None
    | Some White -> 
        state |> Map.add location Black
    | Some Black ->
        state |> Map.add location White

let flipTile state path =
    let location = follow (0,0,0) path
    state |> flip location
    
let paths = input |> Seq.map parsePath |> Seq.toList
let flipped =
    paths
    |> List.fold flipTile Map.empty

let part1 = flipped |> Map.toSeq |> Seq.map snd |> Seq.filter (function | Black -> true | _ -> false) |> Seq.length

printf "Test.."
test <@ parsePath "seswswnwswwwswswenwneesenwswswswnwesw" = [SE; SW; SW; NW; SW; W; W; SW; SW; E; NW; NE; E; SE; NW; SW; SW; SW; NW; E;SW] @>
printfn "..done!"