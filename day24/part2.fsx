#r "nuget: Unquote"
open Swensen.Unquote


//We're using 3D coordinates to represent locations in the hexagonal grid
//https://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MARTIN/Hex.pdf

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Direction = NE | E | SE | SW | W | NW
type Color = White | Black

//ADT representing our grid
type State = (int * int * int) Set //Contains all the black hexes
let colorAt state location =
    match state |> Set.contains location with
    | true -> Black
    | false -> White
let makeBlack = Set.add
let makeWhite = Set.remove
let countBlackTiles state = 
    state |> Set.count

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

let flip location state =
    match colorAt state location with
    | Black -> state |> makeWhite location
    | White -> state |> makeBlack location

let flipTile state path =
    let location = follow (0,0,0) path
    state |> flip location

let neighbours (x,y,z) =
    let offsets = 
        [(1,1,0);(1,0,-1);(0,-1,-1);(-1,-1,0);(-1,0,1);(0,1,1)]
    offsets |> List.map (fun (dx,dy,dz) -> (x+dx,y+dy,z+dz))
    
let applyRules (state : State) (location : int*int*int) : State =
    let nbBlackNeighbours = 
        neighbours location
        |> List.map (colorAt state)
        |> List.filter (function | Black -> true | _ -> false)
        |> List.length
    let hexColor = colorAt state location

    let update =
        match hexColor, nbBlackNeighbours with
        | Black, 0 -> 
            printfn $"making {location} white because it is black and has no black neighbours"
            state |> makeWhite location
        | Black, n when n>2 -> 
            printfn $"making {location} white because it is black and has {n} black neighbours"
            state |> makeWhite location
        | White, 2 -> 
            printfn $"making {location} black because it is white and has 2 black neighbours"
            state |> makeBlack location
        | _ -> state
    update

let nextDay (state : State) =
    let grid = state |> Set.toList
    let hexes = 
        grid @ (grid  |> List.collect neighbours)
        |> Set.ofList
    hexes
    |> Set.fold applyRules state //BUG! we don't want to apply rules on a half-updated tileset!!!

let rec days n state =
    printfn "%d: %d" (100-n) (countBlackTiles state)
    if n = 1
    then state
    else days (n-1) (nextDay state)

let paths = example |> Seq.map parsePath |> Seq.toList
let initial =
    paths
    |> List.fold flipTile Set.empty

initial |> countBlackTiles
initial |> nextDay |> countBlackTiles
initial |> nextDay |> Set.toList

let part2 = days 100 initial |> countBlackTiles

printf "Test.."
test <@ parsePath "seswswnwswwwswswenwneesenwswswswnwesw" = [SE;SW;SW;NW;SW;W;W;SW;SW;E;NW;NE;E;SE;NW;SW;SW;SW;NW;E;SW] @>
test <@ parsePath "enesenwswwswneneswsenwnewswseenwsese" = [E;NE;SE;NW;SW;W;SW;NE;NE;SW;SE;NW;NE;W;SW;SE;E;NW;SE;SE] @>
printfn "..done!"
