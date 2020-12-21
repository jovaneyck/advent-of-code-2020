#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

//quote of the day: https://youtu.be/S4LzzuMTqjs?t=643
//raw part 1 footage, explain why it did 2000x times more work, show part2 outline, dive in!

//let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Recipe = { ingredients : string []; allergens : string [] }

let parse (text : string) =
    let m = Regex("^(.*) \(contains (.*)\)$").Match text
    let ingredients = m.Groups.[1].Value.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)
    let allergens = m.Groups.[2].Value.Split([|", "|], System.StringSplitOptions.None)
    { ingredients = ingredients; allergens = allergens }

let recipes = example |> Seq.map parse
let ingredients = recipes |> Seq.collect (fun l -> l.ingredients) |> Seq.toList
let allergens = recipes |> Seq.collect (fun l -> l.allergens) |> Seq.distinct |> Seq.toList

allergens |> Seq.length 
ingredients |> Seq.distinct |> Seq.length 

let recipesByAllergen =
    allergens
    |> List.map (fun allergen -> 
        recipes 
        |> Seq.filter (fun l -> l.allergens |> Seq.contains allergen)
        |> (fun recipes -> allergen, recipes))
    |> Map.ofSeq

let noMatch ingredient allergen =
    let recipesWithAllergen = recipesByAllergen |> Map.find allergen
    recipesWithAllergen |> Seq.exists (fun recipe -> recipe.ingredients |> Seq.contains ingredient |> not)

let doesNotContainAllergens allergens ingredient =
    allergens
    |> Seq.forall (noMatch ingredient)

#time "on"
let notContainingAllergens = 
    ingredients
    |> Seq.distinct
    |> Seq.indexed
    |> Seq.map (fun (idx, ingredient) -> printfn "working on #%d" (1+idx); ingredient)
    |> Seq.filter (doesNotContainAllergens allergens)
    |> Seq.toList

let inertIngredients = ingredients |> Seq.distinct |> Seq.filter (fun i -> notContainingAllergens |> Seq.contains i) |> Seq.toList

let withoutInert = 
    recipes
    |> Seq.toList
    |> List.map (fun r -> { r with ingredients = r.ingredients |> Seq.except inertIngredients |> Seq.toArray })

printf "Test.."
printfn "..done!"