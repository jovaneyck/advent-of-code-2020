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

let applyColor location color state =
    let makeBlack = Set.add
    let makeWhite = Set.remove

    match color with
    | White -> state |> makeWhite location
    | Black -> state |> makeBlack location

let countBlackTiles state = state |> Set.count

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
    let newColor = 
        match colorAt state location with
        | White -> Black
        | Black -> White
    applyColor location newColor state

let flipTile state path =
    let location = follow (0,0,0) path
    state |> flip location

let neighbours (x,y,z) =
    let offsets = [(1,1,0);(1,0,-1);(0,-1,-1);(-1,-1,0);(-1,0,1);(0,1,1)]
    offsets |> List.map (fun (dx,dy,dz) -> (x+dx,y+dy,z+dz))
    
let nextColorFor (state : State) (location : int*int*int) : Color =
    let nbBlackNeighbours = 
        neighbours location
        |> List.map (colorAt state)
        |> List.filter (function | Black -> true | _ -> false)
        |> List.length
    let hexColor = colorAt state location
    let nextColor =
        match hexColor, nbBlackNeighbours with
        | Black, 0 -> White
        | Black, n when n>2 -> White
        | White, 2 -> Black
        | color,_ -> color
    nextColor

let nextDay (state : State) =
    let hexes = 
        Set.union
            state 
            (state |> Set.toSeq |> Seq.collect neighbours |> Set.ofSeq)
    let updates = hexes |> Seq.map (fun h -> h, nextColorFor state h)
    updates |> Seq.fold (fun state (loc, color) -> applyColor loc color state) state

let rec days n state =
    if n = 0
    then state
    else days (n-1) (nextDay state)

let solve input =
    input
    |> Seq.map parsePath 
    |> Seq.toList 
    |> List.fold flipTile Set.empty 
    |> days 100
    |> countBlackTiles

let part2 = solve input

printf "Test.."
test <@ parsePath "seswswnwswwwswswenwneesenwswswswnwesw" = [SE;SW;SW;NW;SW;W;W;SW;SW;E;NW;NE;E;SE;NW;SW;SW;SW;NW;E;SW] @>
test <@ parsePath "enesenwswwswneneswsenwnewswseenwsese" = [E;NE;SE;NW;SW;W;SW;NE;NE;SW;SE;NW;NE;W;SW;SE;E;NW;SE;SE] @>
test <@ solve example = 2208 @>
test <@ solve input = 4200 @>
printfn "..done!"
