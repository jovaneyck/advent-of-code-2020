#r @"nuget: Unquote"
#r "nuget: FSharp.Text.RegexProvider"
open Swensen.Unquote
open FSharp.Text.RegexProvider

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path

type Password = { min : int; max : int; character: char; password : string }

type PasswordRegex = Regex< @"(?<min>^\d*)-(?<max>\d*)\s(?<character>\w):\s(?<password>\w*)$" >
let parsed =
    input
    |> Seq.map (PasswordRegex().TypedMatch)
    |> Seq.map (fun m -> { min = int m.min.Value; max = int m.max.Value; character = char m.character.Value; password = m.password.Value })
    |> Seq.toList

let isValid (password : Password) = 
    let occurences = password.password |> Seq.filter (fun c -> c = password.character) |> Seq.length
    password.min <= occurences && occurences <= password.max

let valid =
    parsed |> List.filter isValid

Seq.length valid

printf "Testing..."
test <@ 1 + 1 = 2@>
printfn "done!"
