#r "nuget: Unquote"
open Swensen.Unquote

//let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  

type Card = int
type Deck = { player : int; cards : int list }
type GameState = Deck * Deck

let parse (text : string seq) : GameState = 
    let p1 = text |> Seq.takeWhile ((<>) "") |> Seq.skip 1 |> Seq.map int |> Seq.toList
    let p2 = text |> Seq.skipWhile ((<>) "") |> Seq.skip 2 |> Seq.map int |> Seq.toList
    ({ player= 1; cards = p1 }, { player= 2; cards = p2 })

let mutable gameNb = 0L

let rec playRound (p1, p2) : GameState =
    let p1Card :: p1Deck = p1.cards
    let p2Card :: p2Deck = p2.cards

    if (p1Deck |> Seq.length) >= p1Card && (p2Deck |> Seq.length) >= p2Card
    then
        gameNb <- gameNb+1L
        if gameNb % 1_000L = 0L then printfn "starting sub-game %d" gameNb
        let subDeck1 = p1Deck |> List.take p1Card
        let subDeck2 = p2Deck |> List.take p2Card
        let winner = playGame Set.empty ({ p1 with cards = subDeck1}, { p2 with cards = subDeck2})
        match winner.player with
        | 1 -> { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
        | _ -> { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
    else 
        if p1Card > p2Card
        then { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
        else { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
and playGame history ((p1, p2) as state) : Deck =
    match p1.cards, p2.cards with
    | [], _ -> p2
    | _, [] -> p1
    | _ -> 
        let next = state |> playRound 
        if history |> Set.contains next
        then p1
        else playGame (history |> Set.add next) next

let score deck =
    deck 
    |> (fun d -> d.cards) 
    |> Seq.rev 
    |> Seq.mapi (fun idx card -> (idx + 1) * card) 
    |> Seq.reduce (+)

let parsed = parse input
#time "on"
let winner = playGame Set.empty parsed 

//The data structure I was looking for was Queue, but that turned out to be a minor improvement (and incompatible with the real winner outlined below)
//The real win was in using a different structure to keep track of history:
//Immutable sets based on binary trees, where elements are ordered by F# generic comparison. By default comparison is the F# structural comparison
//=> O(logn) lookups/inserts!

//with history LIST: Real: 00:47:48.010, CPU: 00:46:27.718, GC gen0: 507418, gen1: 397, gen2: 29
//with history SET: Real: 00:00:11.265, CPU: 00:00:10.640, GC gen0: 1664, gen1: 82, gen2: 3
//https://github.com/ralphhendriks/aoc2020/blob/main/22/Program.fs

let part2 = score winner

printf "Test.."

printfn "..done!"
