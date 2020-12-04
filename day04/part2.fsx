#r @"nuget: Unquote"
#r "nuget: FSharp.Text.RegexProvider"

open Swensen.Unquote
open FSharp.Text.RegexProvider

let input = System.IO.File.ReadAllText $@"{__SOURCE_DIRECTORY__}\input.txt"

type KVP = { Key : string; Value : string }
type Passport = KVP list
type PassportRegex = Regex<"(?<key>\w*):(?<value>\S*)">
let regex = PassportRegex()

let parsePassport text : Passport = 
    regex.TypedMatches(text)
        |> Seq.map (fun kvp -> {Key = kvp.key.Value; Value = kvp.value.Value})
        |> List.ofSeq

let parse (text : string) =
    text.Split([|"\r\n\r\n"|], System.StringSplitOptions.None)
    |> Seq.map parsePassport

let hasRequiredKeys (passport : Passport) = 
    let requiredKeys = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    
    requiredKeys
    |> Seq.forall (fun k -> passport |> List.exists (fun kvp -> kvp.Key = k))

let yearBetween min max (kvp : KVP) =
    let year = int kvp.Value
    kvp.Value.Length = 4 && min <= year && year <= max

type HeightRegex = Regex<"^(?<value>\d*)(?<unit>(cm|in))$">
let heightRegex = HeightRegex()
let validHeight (h : string) = 
    match heightRegex.TryTypedMatch(h) with
    | None -> false
    | Some m ->
        let h = int m.value.Value
        match m.unit.Value with
        | "cm" -> 150 <= h && h <= 193
        | "in" -> 59 <= h && h <= 76

type HairColor = Regex<"^#([0-9]|[a-f]){6}$">
let validHairColor = HairColor().IsMatch

type EyeColor = Regex<"^amb|blu|brn|gry|grn|hzl|oth$">
let validEyeColor = EyeColor().IsMatch

type PassportId = Regex<"^[0-9]{9}$">
let validPassportId = PassportId().IsMatch 

let validKVP (kvp : KVP) = 
    match kvp.Key with
    | "byr" -> yearBetween 1902 2002 kvp
    | "iyr" -> yearBetween 2010 2020 kvp
    | "eyr" -> yearBetween 2020 2030 kvp
    | "hgt" -> validHeight kvp.Value
    | "hcl" -> validHairColor kvp.Value
    | "ecl" -> validEyeColor kvp.Value
    | "pid" -> validPassportId kvp.Value
    | "cid" -> true

let isValid (passport : Passport) = 
    hasRequiredKeys passport && passport |> List.forall validKVP

let passports = parse input
Seq.length passports
let answer = passports |> Seq.filter isValid |> Seq.length

printf "Testing..."
printfn "done!"
