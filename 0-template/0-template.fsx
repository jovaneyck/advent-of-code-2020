#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $@"{__SOURCE_DIRECTORY__}\input.txt"

printf "Testing..."
test <@ 1 + 1 = 4 @>
printfn "done!"
