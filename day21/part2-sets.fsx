#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

//Solution strategy by: https://www.youtube.com/watch?v=tXwh0y0PyPw
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
//let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Recipe = { ingredients : string Set; allergens : string list }

let parse (text : string) =
    let m = Regex("^(.*) \(contains (.*)\)$").Match text
    let ingredients = 
        m.Groups.[1].Value.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Set.ofSeq
    let allergens = 
        m.Groups.[2].Value.Split([|", "|], System.StringSplitOptions.None) 
        |> Seq.toList
    { ingredients = ingredients; allergens = allergens }

let recipes = input |> Seq.map parse |> Seq.toList

type Mapping = Map<string, string Set> //allergen->possible ingredients

let candidates : Mapping =
    recipes
    |> List.collect (fun recipe -> recipe.allergens |> List.map (fun allergen -> (allergen, recipe.ingredients)))
    |> List.groupBy fst
    |> List.map (fun (allergen, cands) -> allergen, cands |> List.map snd |> List.reduce Set.union)
    |> Map.ofSeq

let processRecipe mapping recipe =
    recipe.allergens
    |> Seq.fold 
        (fun mapping allergen -> 
            let current = mapping |> Map.find allergen
            let filtered = Set.intersect current recipe.ingredients
            mapping |> Map.add allergen filtered)
        mapping

let filteredMapping =
    recipes
    |> List.fold processRecipe candidates
    
let rec reduce acc (mapping : (string * string Set) list) =
    match mapping with
    | [] -> acc
    | _ ->
        let nextSolvedMapping = 
            mapping 
            |> List.find (fun (_, ingredients) -> ingredients |> Seq.length = 1)
        let allergen = fst nextSolvedMapping
        let ingredient = snd nextSolvedMapping |> Seq.head
        let nextMap =
            mapping 
            |> List.map (fun (allergen, ingredients) -> (allergen, Set.difference ingredients (Set.singleton ingredient)))
            |> List.filter (fun (_, ingredients) -> ingredients |> Set.isEmpty |> not)
        reduce ((allergen, ingredient) :: acc) nextMap

let finalMapping = reduce [] (filteredMapping |> Map.toList)
let part2 = finalMapping |> List.sortBy fst |> List.map snd |> String.concat ","
//val part2 : string = "dpkvsdk,xmmpt,cxjqxbt,drbq,zmzq,mnrjrf,kjgl,rkcpxs"