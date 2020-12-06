#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllText $@"{__SOURCE_DIRECTORY__}\input.txt"
let groups = input.Split([|"\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
groups |> Seq.map (fun g -> g |> Seq.distinct |> Seq.except ['\r'; '\n']) |> Seq.sumBy Seq.length

printf "Test.."

test <@ 1 = 1 + 1 @>

printfn "done!"