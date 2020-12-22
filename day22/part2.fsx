#r "nuget: Unquote"
open Swensen.Unquote

let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  
//let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  

type Card = int
type Deck = { player : int; cards : int list }
type GameState = Deck * Deck

let parse (text : string seq) : GameState = 
    let p1 = text |> Seq.takeWhile ((<>) "") |> Seq.skip 1 |> Seq.map int |> Seq.toList
    let p2 = text |> Seq.skipWhile ((<>) "") |> Seq.skip 2 |> Seq.map int |> Seq.toList
    ({ player= 1; cards = p1 }, { player= 2; cards = p2 })

let playRound (p1, p2) : GameState =
    let p1Card :: p1Deck = p1.cards
    let p2Card :: p2Deck = p2.cards

    if p1Card > p2Card
    then { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
    else { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
    

let rec playGame ((p1, p2) as state) : Deck =
    match p1.cards, p2.cards with
    | [], _ -> p2
    | _, [] -> p1
    | _ -> state |> playRound |> playGame

let score deck =
    deck 
    |> (fun d -> d.cards) 
    |> Seq.rev 
    |> Seq.mapi (fun idx card -> (idx + 1) * card) 
    |> Seq.reduce (+)

let parsed = parse example
let winner = playGame parsed
let part1 = score winner

printf "Test.."

printfn "..done!"