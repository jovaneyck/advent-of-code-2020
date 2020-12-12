#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Action = N | E | S | W | L | R | F
type Instruction = { action : Action; value : int }
type Orientation = North | East | South | West
type State = { location : int*int; orientation : Orientation }

let initial = { location = (0,0); orientation = East }

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
let rotate rotation state value =
    let newOrientation =
        match rotation, state.orientation, value with
        | _, North, 180 -> South
        | _, East, 180 -> West
        | _, South, 180 -> North
        | _, West, 180 -> East

        | Left, North, 90 -> West
        | Left, North, 270 -> East
        | Right, North, 90 -> East
        | Right, North, 270 -> West

        | Left, East, 90 -> North
        | Left, East, 270 -> South
        | Right, East, 90 -> South
        | Right, East, 270 -> North
    
        | Left, South, 90 -> East
        | Left, South, 270 -> West
        | Right, South, 90 -> West
        | Right, South, 270 -> East
    
        | Left, West, 90 -> South
        | Left, West, 270 -> North
        | Right, West, 90 -> North
        | Right, West, 270 -> South
    { state with orientation = newOrientation }

let forward state value = 
    let (x,y) = state.location
    let newLocation =
        match state.orientation with
        | North -> (x,y+value)
        | East -> (x+value,y)
        | South -> (x,y-value)
        | West -> (x-value,y)

    { state with location = newLocation }

let move orientation state value = 
    let (x,y) = state.location
    let nextLocation =
        match orientation with
        | North -> (x,y+value)
        | East  -> (x+value,y)
        | West  -> (x-value,y)
        | South -> (x,y-value)
    { state with location = nextLocation }

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
1109 + 24
printf "Test.."
test <@ 1+1=2 @> 
printfn "done!"