#r "nuget: Unquote"
open Swensen.Unquote

let input = "942387615"
let parsed = (input |> Seq.map (string>>int) |> Seq.toList) @ [10..1_000_000]

type LinkedList = { value : int; mutable next : LinkedList option }

let take n node =
    let rec take acc n node =
        if n = 0 then acc |> List.rev
        else take (node.value :: acc) (n-1) (node.next.Value)
    take [] n node

let rec find n node =
    if node.value = n then node
    else find n (node.next.Value)

let toLinkedList list =
    let nodes = list |> List.map (fun n -> { value = n; next = None})
    //Lets's link
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
    //printfn "figuring out destination given %d %d %A" maximum current selected
    if selected |> Seq.contains current
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
            let destination = findDestination maximum ((current.value - 1) |> wrap maximum) [one.value;two.value;three.value]
            //printfn "cups: %A" (take 9 current)
            //printfn "pick up: %d %d %d" one.value two.value three.value
            //printfn "destination: %A" destination

            let destNode = valuesToNodes |> Map.find destination
            current.next <- Some rest
            three.next <- destNode.next
            destNode.next <- Some one

            move (n-1) rest
    move n current

#time "on"
let final = move 10_000_000 start

let one = find 1 final
let part2 = (int64 one.next.Value.value) * (int64 one.next.Value.next.Value.value)