#r @"nuget: Unquote"
#r "nuget: FSharp.Text.RegexProvider"

open Swensen.Unquote
open FSharp.Text.RegexProvider

let input = System.IO.File.ReadAllText $@"{__SOURCE_DIRECTORY__}\input.txt"
let example = @"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

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

let requiredKeys = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let hasRequiredKeys (passport : Passport) = 
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
let hairColorRegex = HairColor()
let validHairColor (text : string) =
    match hairColorRegex.TryTypedMatch(text) with
    | Some _ -> true
    | _ -> false

let validEyeColor text =
    ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    |> List.exists ((=) text)

type PassportId = Regex<"^[0-9]{9}$">
let passportIdRegex = PassportId()
let validPassportId text = 
    match passportIdRegex.TryTypedMatch text with
    | Some _ -> true
    | _ -> false

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
passports |> Seq.filter isValid |> Seq.length

printf "Testing..."
printfn "done!"
