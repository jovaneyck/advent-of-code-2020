#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt" |> Seq.map int64 |> Seq.toList  
let example = [5764801L; 17807724L]

let initial_subject_number = 7L
let modul = 20201227L

let next value subject_number = 
    (value * subject_number) % modul

let public_keys =
    let rec keys n =
        let nnn = next n initial_subject_number
        seq {
            yield nnn
            yield! keys nnn
        }
    keys 1L 
    |> Seq.cache

let encrypt subject_number loop_size =
    let rec encrypt value loop_size =
        if loop_size = 0L then value
        else encrypt (next value subject_number) (loop_size - 1L)
    encrypt 1L loop_size

let find_loop_size public_key =
    1 + (public_keys |> Seq.findIndex ((=) public_key)) |> int64

let card_loop_size =  find_loop_size input.[1]
let result = encrypt input.[0] card_loop_size //9714832

printf "Test.."
printfn "..done!"