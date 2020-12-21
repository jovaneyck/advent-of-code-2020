#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

//quote of the day: https://youtu.be/S4LzzuMTqjs?t=643
let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
//let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Recipe = { ingredients : string list; allergens : string list }

let parse (text : string) =
    let m = Regex("^(.*) \(contains (.*)\)$").Match text
    let ingredients = m.Groups.[1].Value.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList
    let allergens = m.Groups.[2].Value.Split([|", "|], System.StringSplitOptions.None) |> Seq.toList
    { ingredients = ingredients; allergens = allergens }

let recipes = input |> Seq.map parse |> Seq.toList
let ingredients = recipes |> List.collect (fun l -> l.ingredients)
let allergens = recipes |> List.collect (fun l -> l.allergens) |> List.distinct

allergens |> Seq.length 
ingredients |> Seq.distinct |> Seq.length 

let recipesByAllergen =
    allergens
    |> List.map (fun allergen -> 
        recipes 
        |> List.filter (fun l -> l.allergens |> List.contains allergen)
        |> (fun recipes -> allergen, recipes))
    |> Map.ofSeq

let noMatch ingredient allergen =
    let recipesWithAllergen = recipesByAllergen |> Map.find allergen
    recipesWithAllergen |> List.exists (fun recipe -> recipe.ingredients |> List.contains ingredient |> not)

let doesNotContainAllergens allergens ingredient =
    allergens
    |> List.forall (noMatch ingredient)

let notContainingAllergens = 
    ingredients
    |> List.distinct
    |> List.filter (doesNotContainAllergens allergens)

let inertIngredients = ingredients |> List.distinct |> List.filter (fun i -> notContainingAllergens |> List.contains i)

let recipesWithoutIntert = 
    recipes
    |> Seq.toList
    |> List.map (fun r -> { r with ingredients = r.ingredients |> List.except inertIngredients })

let candidatesPerAllergen =
    allergens
    |> List.map (fun allergen -> 
        (allergen, 
         recipesWithoutIntert 
         |> List.filter (fun r -> r.allergens |> Seq.contains allergen)
         |> List.collect (fun r -> r.ingredients)
         |> List.distinct))
    |> Map.ofList

//Let's search for a valid allergen->ingredient mapping!
let validMapping allergen ingredient = 
    let recipesWithAllergen = recipesByAllergen |> Map.find allergen
    recipesWithAllergen |> List.forall (fun recipe -> recipe.ingredients |> List.contains ingredient)

let rec tryOptions acc candidatesPerAllergen ingredients allergen =
    //printfn "We have multiple options for %s: %A" allergen ingredients
    match ingredients with
    | [] -> 
        //printfn "Uh oh, we have exhausted our options!"
        None
    | i :: is -> 
        //more than one option for this allergen, we need to do a bit of searching, just trying a naive DFS
        //assume allergen = i
        let nextLookup = 
            candidatesPerAllergen 
            |> Map.remove allergen
            |> Map.map (fun _ ingrs -> ingrs |> List.except [i])
        let nextAcc = (allergen, i) :: acc
        //printfn "We'll try option %s for %s" i allergen
        if validMapping allergen i |> not 
        then tryOptions acc candidatesPerAllergen is allergen
        else 
            match findMatches nextAcc nextLookup with
            | Some answer -> Some answer
            | None -> tryOptions acc candidatesPerAllergen is allergen
and findMatches acc candidatesPerAllergen =
        if candidatesPerAllergen |> Map.isEmpty 
        then Some acc
        else
            let mostConstrained =
                candidatesPerAllergen 
                |> Map.toSeq 
                |> Seq.sortBy (fun (allergen, ingredients) -> ingredients |> Seq.length) 
                |> Seq.head
            let allergen, ingredients = mostConstrained
            //printfn "%s is the most constrained allergen with options: %A" allergen ingredients
            match ingredients with
            | [] -> None
            | [i] ->
                //printfn "%s can only match with %s, easy!" allergen i
                //only one option, we found a match for free!
                let nextAcc = (allergen, i) :: acc
                let nextLookup = 
                    candidatesPerAllergen 
                    |> Map.remove allergen
                    |> Map.map (fun _ ingrs -> ingrs |> List.except [i])
                if validMapping allergen i |> not 
                then None
                else findMatches nextAcc nextLookup
            | ingredients -> tryOptions acc candidatesPerAllergen ingredients allergen

let matches = findMatches [] candidatesPerAllergen
let sorted = 
    matches 
    |> Option.defaultValue []
    |> List.sortBy (fun (allergen, ingredient) -> allergen)
let part2 = sorted |> List.map snd |> String.concat ","

printf "Test.."
printfn "..done!"