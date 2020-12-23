#r "nuget: Unquote"
open Swensen.Unquote

let input = "942387615"
let parsed = input |> Seq.map (string>>int) |> Seq.toList

let wrap max n =
    if n = 0 
    then max
    else 1 + (n - 1) % max

let rec findDestination maximum current selected =
    printfn "figuring out destination given %d %d %A" maximum current selected
    if selected |> Seq.contains current
    then findDestination maximum ((current - 1) |> wrap maximum) selected
    else current

let move n cups =
    let maximum = parsed |> List.max
    let length = parsed |> List.length
    let rec move n cups =
        if n = 0 
        then cups
        else
            let (current :: one :: two :: three :: rest) = cups
            printfn "cups: %A" cups

            let destination = findDestination maximum ((current - 1) |> wrap maximum) [one;two;three]
            printfn "destination: %A" destination

            let destinationIndex = rest @ [current] |> List.findIndex ((=) destination)
            let before,after = rest @ [current] |> List.splitAt (destinationIndex + 1)

            let next = before @ [one;two;three] @ after
            move (n-1) next
    move n cups

let score cups =
    let oneIdx = cups |> List.findIndex ((=) 1)
    let before,after = cups |> List.splitAt oneIdx
    (List.tail after @ before) |> Seq.map string |> String.concat ""

let moved = move 100 parsed
let part1 = score moved

printf "Test.."
test <@ findDestination 9 2 [1;2;8] = 9 @>
test <@ findDestination 9 1 [3;6;7] = 1 @>
test <@ score [8; 3; 7; 4; 1; 9; 2; 6; 5] = "92658374" @>
printfn "..done!"