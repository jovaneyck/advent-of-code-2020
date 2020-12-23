#r "nuget: Unquote"
open Swensen.Unquote

let input = "942387615"
let parsed = (input |> Seq.map (string>>int) |> Seq.toList) @ [10..1_000_000]

type LinkedList = { value : int; mutable next : LinkedList option }

let toLinkedList list =
    let nodes = list |> List.map (fun n -> { value = n; next = None})
    
    nodes 
    |> List.pairwise
    |> List.iter (fun (f,s) -> f.next <- Some s)

    (nodes |> Seq.last).next <- Some (nodes |> Seq.head)

    nodes

let linked = toLinkedList parsed
let valuesToNodes = linked |> List.map (fun n -> (n.value, n)) |> Map.ofList
let start = linked |> Seq.head

let wrap max n =
    if n = 0 
    then max
    else 1 + (n - 1) % max

let rec findDestination maximum current selected =
    if selected |> List.contains current
    then findDestination maximum ((current - 1) |> wrap maximum) selected
    else current

let move n (current : LinkedList) =
    let maximum = parsed |> List.max
    let length = parsed |> List.length
    let rec move n (current : LinkedList) =
        if n % 1_000_000 = 0 then printfn "moves: %d" n
        if n = 0 
        then current
        else
            let one = current.next.Value
            let two = one.next.Value
            let three = two.next.Value
            let rest = three.next.Value
            let destination = 
                findDestination 
                    maximum 
                    ((current.value - 1) |> wrap maximum) 
                    [one.value;two.value;three.value]

            let destNode = valuesToNodes |> Map.find destination
            current.next <- Some rest
            three.next <- destNode.next
            destNode.next <- Some one

            move (n-1) rest
    move n current

#time "on"
let final = move 10_000_000 start

let one = valuesToNodes |> Map.find 1
let part2 = (int64 one.next.Value.value) * (int64 one.next.Value.next.Value.value)