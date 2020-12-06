#r @"nuget: Unquote"
open Swensen.Unquote

let split (separator : string) (s:string) =
    s.Split([|separator|], System.StringSplitOptions.RemoveEmptyEntries)   

let input = System.IO.File.ReadAllText $@"{__SOURCE_DIRECTORY__}\input.txt"
let groups = split "\r\n\r\n" input
groups |> Seq.map (fun g -> g |> split "\r\n" |> Seq.map Set.ofSeq |> Set.intersectMany |> Seq.length) |> Seq.sum

printf "Test.."

test <@ 1 = 1 + 1 @>

printfn "done!"