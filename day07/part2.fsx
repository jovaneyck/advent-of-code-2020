#r @"nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $@"{__SOURCE_DIRECTORY__}\input.txt"
type Rule = string * (int * string) list
let parse rule : Rule =
    let m = (new Regex("^(?<bag>.*) bags contain (?<rules>.*)\.$")).Match(rule)
    let bag = m.Groups.["bag"].Value
    let ruleStrings = m.Groups.["rules"].Value.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries)
    let parseRightHand (s : string) = 
        if s = "no other bags" then
            None
        else
            let words = s.Split([|' '|])
            Some (int words.[0], words.[1..words.Length - 2] |> String.concat " " )
    bag, ruleStrings |> Seq.choose parseRightHand |> List.ofSeq

let rec findBagsThatCanContain rulebook bag =
    let applies bag (rule : Rule) = snd rule |> List.map snd |> List.contains bag
    let containerBags = rulebook |> List.filter (applies bag) |> List.map fst
    containerBags @ (containerBags |> List.collect (findBagsThatCanContain rulebook))

let rec numberOfBagsIn rulebook bag = 
    let containingBags = rulebook |> List.find (fun r -> fst r = bag) |> snd
    containingBags |> List.sumBy (fun (nb, b) -> nb + (nb * (numberOfBagsIn rulebook b)))

let rulebook = input |> Seq.map parse |> List.ofSeq

printf "Test.."
test <@ parse "light red bags contain 1 bright white bag, 2 muted yellow bags." =  ("light red", [(1, "bright white"); (2, "muted yellow")]) @>
test <@ findBagsThatCanContain rulebook "shiny gold" |> Set.ofSeq |> Seq.length = 112 @>
test <@ numberOfBagsIn rulebook "shiny gold" = 6260 @>
printfn "done!"