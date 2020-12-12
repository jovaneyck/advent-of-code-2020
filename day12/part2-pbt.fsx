#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Action = N | E | S | W | L | R | F
type Instruction = { action : Action; value : int }
type Orientation = North | East | South | West
type State = { location : int*int; waypoint : int*int }

let initial = { location = (0,0); waypoint = (10,1) }

let parse text =
    let parseLine (line : string) =
        let action : Action =
            match line.[0] with
            | 'N' -> N 
            | 'E' -> E 
            | 'S' -> S 
            | 'W' -> W 
            | 'L' -> L 
            | 'R' -> R 
            | 'F' -> F
            | u -> failwith $"Unknown action: {u}"
        let value = line.[1..] |> int
        { action = action; value = value }
    text |> Seq.map parseLine

type Rotation = Left | Right

let rotateWaypoint rotation degrees waypoint =
    let (x,y) = waypoint
    match rotation, degrees with
    | Right, 90 -> (y,-1*x)
    | Left, 90 -> (-1*y,x)
    | _, 180 -> (-1*x,-1*y)
    | Right, 270 -> (-1*y,x)
    | Left, 270 -> (y,-1*x)

let rotate rotation state value =
    let newWaypoint = rotateWaypoint rotation value state.waypoint
    { state with waypoint = newWaypoint }

let forward state value = 
    let (x,y) = state.location
    let (dx,dy) = state.waypoint
    let newLocation = (x + value*dx, y + value*dy)

    { state with location = newLocation }

let move orientation state value = 
    let (x,y) = state.waypoint
    let nextLocation =
        match orientation with
        | North -> (x,y+value)
        | East  -> (x+value,y)
        | West  -> (x-value,y)
        | South -> (x,y-value)
    { state with waypoint = nextLocation }

let execute state instruction =
    match instruction.action with
    | L -> rotate Left state instruction.value
    | R -> rotate Right state instruction.value
    | F -> forward state instruction.value
    | N -> move North state instruction.value
    | E -> move East state instruction.value
    | S -> move South state instruction.value
    | W -> move West state instruction.value

let run instructions =
    instructions |> Seq.fold execute initial

let instructions = parse input
let final = run instructions
24352 + 36701

//Great FsCheck introduction: https://fsharpforfunandprofit.com/posts/property-based-testing/
#r "nuget: FsCheck"
open FsCheck

let qc name prop = 
    Check.One (name, { FsCheck.Config.Default with QuietOnSuccess = true }, prop)

printf "Test.."
test <@ forward { initial with waypoint = (10,1); location = (0,0) } 10 
            = { initial with waypoint = (10,1); location = (100,10) } @>
test <@ rotateWaypoint Right 90 (10,4) = (4, -10) @> 
test <@ rotateWaypoint Right 90 (4,-10) = (-10,-4) @> 
test <@ rotateWaypoint Right 90 (-10,-4) = (-4, 10) @> 
test <@ rotateWaypoint Right 90 (-4, 10) = (10,4) @> 
test <@ rotateWaypoint Left 90 (4, -10) = (10,4) @> 
test <@ rotateWaypoint Right 180 (10,4) = (-10, -4) @> 
test <@ rotateWaypoint Right 270 (4, -10) = (10,4) @> 

qc 
    "rotating left or right 180 ends up the same" 
    (fun coord -> rotateWaypoint Left 180 coord = rotateWaypoint Right 180 coord)
qc 
    "rotating left 270 ends up in the same place as right 90" 
    (fun coord -> rotateWaypoint Left 270 coord = rotateWaypoint Right 90 coord)
qc 
    "rotating left 90 ends up in the same place as right 270" 
    (fun coord -> rotateWaypoint Left 90 coord = rotateWaypoint Right 270 coord)
qc 
    "rotating 90 twice in the same direction ends up in a 180 turn" 
    (fun rotation coord -> 
        coord 
        |> rotateWaypoint rotation 90
        |> rotateWaypoint rotation 90
            = rotateWaypoint rotation 180 coord)
qc 
    "rotating 90 four times in the same direction ends up in a 360 turn" 
    (fun rotation coord -> 
        coord 
        |> rotateWaypoint rotation 90
        |> rotateWaypoint rotation 90
        |> rotateWaypoint rotation 90
        |> rotateWaypoint rotation 90
            = coord)

printfn "done!"