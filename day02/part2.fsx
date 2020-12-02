#r @"nuget: Unquote"
#r "nuget: FSharp.Text.RegexProvider"
open Swensen.Unquote
open FSharp.Text.RegexProvider

let path = $@"{__SOURCE_DIRECTORY__}\input.txt"
let input = System.IO.File.ReadAllLines path

type Password = { pos1 : int; pos2 : int; character: char; password : string }

type PasswordRegex = Regex< @"(?<pos1>^\d*)-(?<pos2>\d*)\s(?<character>\w):\s(?<password>\w*)$" >
let parsed =
    input
    |> Seq.map (PasswordRegex().TypedMatch)
    |> Seq.map (fun m -> { pos1 = int m.pos1.Value; pos2 = int m.pos2.Value; character = char m.character.Value; password = m.password.Value })
    |> Seq.toList

let isValid (password : Password) = 
    let at1 = password.password.[password.pos1 - 1]
    let at2 = password.password.[password.pos2 - 1]
    (at1 = password.character && at2 <> password.character) || (at2 = password.character && at1 <> password.character)

let valid =
    parsed |> List.filter isValid

Seq.length valid

printf "Testing..."
test <@ 1 + 1 = 2@>
printfn "done!"
