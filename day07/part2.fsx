#r @"nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let example = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.".Split('\n')

let input = System.IO.File.ReadAllLines $@"{__SOURCE_DIRECTORY__}\input.txt"
type Rule = string * (int * string) list
let parse rule : Rule =
    let m = (new Regex("^(?<bag>.*) bags contain (?<rules>.*).$")).Match(rule)
    let bag = m.Groups.["bag"].Value
    let ruleStrings = m.Groups.["rules"].Value.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries)
    let parseRightHand (s : string) = 
        if s = "no other bags" then
            None
        else
            let words = s.Split([|' '|])
            Some (int words.[0], words.[1..words.Length - 2] |> String.concat " " )
    bag, ruleStrings |> Seq.choose parseRightHand |> List.ofSeq

let applies bag (rule : Rule) = 
    snd rule |> List.map snd |> List.contains bag
let rulebook = input |> Seq.map parse |> List.ofSeq

let rec findBagsThatCanContain bag =
    let containerBags = rulebook |> List.filter (applies bag) |> List.map fst
    match containerBags with
    | [] -> []
    | bags -> 
        let distinct = bags |> List.distinct
        distinct @ (distinct |> List.collect findBagsThatCanContain)

findBagsThatCanContain "shiny gold" |> Set.ofSeq |> Seq.length

let rec numberOfBagsIn bag = 
    let containingBags = rulebook |> List.find (fun r -> fst r = bag) |> snd
    match containingBags with
    | [] -> 0
    | bags -> bags |> List.sumBy (fun (nb, b) -> nb + (nb * (numberOfBagsIn b)))

numberOfBagsIn "shiny gold"

printf "Test.."
test <@ parse "light red bags contain 1 bright white bag, 2 muted yellow bags." =  ("light red", [(1, "bright white"); (2, "muted yellow")]) @>
printfn "done!"