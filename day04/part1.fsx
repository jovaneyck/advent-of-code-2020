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
let isValid (passport : Passport) = 
    requiredKeys
    |> Seq.forall (fun k -> passport |> List.exists (fun kvp -> kvp.Key = k))

let passports = parse input
Seq.length passports
passports |> Seq.filter isValid |> Seq.length

printf "Testing..."
printfn "done!"
