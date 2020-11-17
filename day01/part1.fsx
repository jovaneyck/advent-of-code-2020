#r @"nuget: Unquote"
open Swensen.Unquote

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path

printf "Testing..."
test <@ 1 + 1 = 4 @>
printfn "done!"
