#r @"nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example =  System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\examplepart2.txt" 

type Ticket = int list

//parse
let parseTicket (text : string) =
    text.Split([|','|]) |> Seq.map int |> Seq.toList

type Rule = { field : string; ranges : (int*int) list }

let parseRule (text : string) =
    let ruleRegex = Regex("(.*): (\d*)-(\d*) or (\d*)-(\d*)")
    let m = ruleRegex.Match(text)
    let field = m.Groups.[1].Value
    let fl = m.Groups.[2].Value |> int
    let fu = m.Groups.[3].Value |> int
    let sl = m.Groups.[4].Value |> int
    let su = m.Groups.[5].Value |> int
    { field = field; ranges = [(fl,fu); (sl,su) ] }

type ParseResult = { rules : Rule list; ourTicket : Ticket; nearbyTickets : Ticket list }
let parse input =
    let noEmptyLines = input |> Seq.filter (fun l -> l <> "")
    let rules = noEmptyLines |> Seq.takeWhile (fun s -> s <> "your ticket:") |> Seq.map parseRule |> Seq.toList
    let ourTicket = noEmptyLines |> Seq.skip ((rules |> Seq.length) + 1) |> Seq.head |> parseTicket
    let nearbyTickets = noEmptyLines |> Seq.skipWhile ( (<>) "nearby tickets:" ) |> Seq.skip 1 |> Seq.map parseTicket |> Seq.toList
    { rules = rules; ourTicket = ourTicket; nearbyTickets = nearbyTickets }

let applies (rule : Rule) number =
    rule.ranges |> Seq.exists (fun (min,max) -> min <= number && number <= max)

let isValid rules ticket = 
    ticket
    |> Seq.forall (fun number -> rules |> Seq.exists (fun rule -> applies rule number))

let findPassingRulesFor rules tickets idx =
    let values = tickets |> Seq.map (Seq.item idx)
    let passingRules = 
        rules 
        |> Seq.filter (fun rule -> values |> Seq.forall(fun v -> applies rule v)) 
        |> Seq.toList
    match passingRules with
    | [rule] -> Some (idx,rule)
    | [] -> failwith $"No rule found matching index {idx}"
    | _ -> None

let rec rulesForFields indices rules tickets =
    match indices with
    | [] -> []
    | _ ->
        let (idx, rule) =
            indices
            |> List.pick (findPassingRulesFor rules tickets)
        (idx, rule) :: (rulesForFields (indices |> List.except [idx]) (rules |> List.except [rule]) tickets)

let parsed = parse input

let validNearbyTickets = 
    parsed.nearbyTickets 
    |> Seq.filter (fun ticket -> ticket |> (isValid parsed.rules))
let indices = parsed.ourTicket |> Seq.indexed |> Seq.map fst |> Seq.toList

let rules = rulesForFields indices parsed.rules validNearbyTickets
let departureIndices = rules |> List.filter (fun (idx,rule) -> rule.field.StartsWith("departure")) |> List.map fst
let part2 = departureIndices |> Seq.map (fun i -> parsed.ourTicket.[i]) |> Seq.map int64 |> Seq.reduce (*)

printf "Test.."
test <@ parseRule "departure location: 37-479 or 485-954" = { field = "departure location"; ranges = [(37, 479); (485, 954)] }@>
printfn "..done!"