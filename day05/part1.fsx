#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $@"{__SOURCE_DIRECTORY__}\input.txt"

type State = { rowMin : int; rowMax : int; colMin : int; colMax : int}
let makeState rmin rmax cmin cmax = { rowMin = rmin; rowMax = rmax; colMin = cmin; colMax = cmax }
let takeStep state character = 
    match character with
    | 'F' ->
        let mid = (state.rowMax - state.rowMin) / 2
        {state with rowMax = (state.rowMin + mid )}
    | 'B' ->
        let mid = (state.rowMax - state.rowMin) / 2
        {state with rowMin = state.rowMin + mid + 1 }
    | 'R' ->
        let mid = (state.colMax - state.colMin) / 2
        {state with colMin = state.colMin + mid + 1 }
    | 'L' ->
        let mid = (state.colMax - state.colMin) / 2
        {state with colMax = (state.colMin + mid )}
    | _ -> failwith "Unknown character in input!"

let decode text =
    let s = Seq.fold takeStep { rowMin = 0; rowMax = 127; colMin = 0; colMax = 7 } text
    ( s.rowMin, s.colMax )

printf "Test.."
test <@ takeStep (makeState 0 127 0 7) 'F' = (makeState 0 63 0 7) @>
test <@ takeStep (makeState 0 63 0 7) 'B' = (makeState 32 63 0 7) @>
test <@ takeStep (makeState 32 63 0 7) 'F' = (makeState 32 47 0 7) @>
test <@ takeStep (makeState 32 47 0 7) 'B' = (makeState 40 47 0 7) @>
test <@ takeStep (makeState 40 47 0 7) 'B' = (makeState 44 47 0 7) @>
test <@ takeStep (makeState 44 47 0 7) 'F' = (makeState 44 45 0 7) @>
test <@ takeStep (makeState 44 45 0 7) 'F' = (makeState 44 44 0 7) @>
test <@ takeStep (makeState 44 44 0 7) 'R' = (makeState 44 44 4 7) @>

test <@ decode "FBFBBFFRLR" = (44,5) @>

printfn "done!"

input |> Seq.map (decode >> (fun (r,c) -> r * 8 + c)) |> Seq.max