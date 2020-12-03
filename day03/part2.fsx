#r @"nuget: Unquote"
open Swensen.Unquote

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path

type Space = Tree | Empty
type Forest = Space list list

let parse (lines : string seq) =
    [
        for row in lines ->
        [
            for s in row ->
                (match s with
                | '.' -> Empty
                | '#' -> Tree
                | _ -> failwith "unknown input")
        ]
    ]
  
let elementAt (forest : Forest) (x,y) =
    if y >= forest.Length then
        None
    else
        let row = forest.[y]
        Some row.[x % row.Length]

type State = { location : int * int; visited : Space list }

let takeStep (forest : Forest) (slope : int * int) (state : State) = 
    let (x,y) = state.location
    let newLocation = (x + (fst slope), y + (snd slope))
    let element = elementAt forest newLocation
    
    element 
    |> Option.map (fun el ->
        let next = { state with location = newLocation; visited = el :: state.visited }
        next, next)

let forest = input |> parse

let run slope =
    let endState = Seq.unfold (takeStep forest slope) { location = (0,0); visited = []} |> Seq.last 
    endState.visited |> List.filter (function | Tree -> true | _ -> false) |> Seq.length

let hits = [(1,1);(3,1);(5,1);(7,1);(1,2)] |> List.map run
hits |> List.map int64 |> List.reduce (*)
let testForest = [[Empty; Tree]; [Tree; Tree]]   
printf "Testing..."
test <@ parse [".#";"##"] = [[Empty; Tree]; [Tree; Tree]] @>
test <@ elementAt testForest (0,0) = Some Empty @>
test <@ elementAt testForest (1,1) = Some Tree @>
test <@ elementAt testForest (3,0) = Some Tree @>
test <@ elementAt testForest (2,0) = Some Empty @>
test <@ elementAt testForest (3,2) = None @>

printfn "done!"
