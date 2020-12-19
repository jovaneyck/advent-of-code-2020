#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\second-example.txt"  

type Rule =
    | Char of char
    | Sequence of int list
    | Alternative of int list list
type Rulebook = Map<int,Rule>
let charRegex = Regex("^(\d*): \"(.)\"$")
let (|CharRule|_|) line =
    let m = charRegex.Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rule = m.Groups.[2].Value |> char
        Some (ruleNumber, rule)
    else
        None
   
let sequenceRegex = Regex("^(\d*): ((\d+)\s?)+$")
let (|SeqRule|_|) line =
    let m = sequenceRegex.Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rules = [for capt in m.Groups.[2].Captures -> capt.Value |> int]
        Some (ruleNumber, rules)
    else
        None 

let choiceRegex = Regex("^(\d*): (.*) \| (.*)$")
let line = "13: 116 113 | 112 110"
let (|AlternativeRule|_|) line =
    let m = choiceRegex.Match(line)
    if m.Success 
    then 
        let left = m.Groups.[2].Value.Split([|' '|]) |> Seq.map int |> Seq.toList
        let right = m.Groups.[3].Value.Split([|' '|]) |> Seq.map int |> Seq.toList
        let ruleNumber = m.Groups.[1].Value |> int
        let rules = [ left; right ]
        Some (ruleNumber, rules)
     else
         None 

let parseRule line =
    match line with
    | CharRule (nb, c) -> (nb, Char c)
    | AlternativeRule (nb, r) -> (nb, Alternative r)
    | SeqRule (nb, r) -> (nb, Sequence r)
    | unknown -> failwith $"Failed to parse rule: {unknown}"
    
let parse text : Rulebook * string list =
    let rulebook =
        text
        |> Seq.takeWhile ((<>) "")
        |> Seq.map parseRule
        |> Map.ofSeq
    let messages =
        text
        |> Seq.skipWhile ((<>) "")
        |> Seq.skip 1
        |> Seq.toList
    (rulebook, messages)

let rec parses (rulebook : Rulebook) rule (message : string) : Result<string,string> =
    printfn $"rule {rule} on {message}"
    let result = 
        match rule with
        | Char c -> 
            if message.StartsWith(string c)
            then message.Substring(1) |> Ok
            else message |> Error
        | Sequence rules ->
            rules 
            |> Seq.map (fun r -> rulebook |> Map.find r) 
            |> Seq.fold 
                (fun result rule -> 
                    match result with
                    | Ok remainder -> parses rulebook rule remainder
                    | err -> err)
                (Ok message)
        | Alternative alternatives ->
            let [first;second] = alternatives |> List.map (fun s -> Sequence s)
            let firstResult = parses rulebook first message
            match firstResult with
            | Ok remainder -> Ok remainder
            | Error _ -> parses rulebook second message
    printfn "which resulted in: %A" result
    result

let applies rulebook rule message = 
    match parses rulebook rule message with
    | Ok "" -> true
    | _ -> false

let (rulebook, messages) = parse example
let updatedRulebook =
    rulebook
    |> Map.add 8 (Alternative [[42]; [42;8]])
    |> Map.add 11 (Alternative [[42;31]; [42;11;31]])

let rule0 = updatedRulebook |> Map.find 0
let valid =
    messages
    |> Seq.filter (applies updatedRulebook rule0)
    |> Seq.toList
valid |> Seq.contains "aaaaabbaabaaaaababaa"
//|> Seq.length

parses updatedRulebook rule0 "aaaaabbaabaaaaababaa"

printf "Test.."
test <@ parseRule @"4: ""a""" = (4, Char 'a') @>
test <@ parseRule "125: 100 116" = (125, Sequence [100; 116]) @>
test <@ parseRule "126: 127 41 | 116 112" = (126, Alternative [[127; 41]; [116; 112]]) @>
test <@ parseRule "13: 116 | 127" = (13, Alternative [[116]; [127]]) @>

test <@ parses Map.empty (Char 'z') "z" = Ok "" @>
test <@ parses Map.empty (Char 'a') "z" = Error "z"@>
test <@ parses Map.empty (Char 'a') "az" = Ok "z" @>

test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "ab" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "abc" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "cab" = (Error "cab") @>

test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "a" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "b" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "c" = (Error "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "ab" = (Ok "b") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "abc" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "bac" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "cab" = (Error "cab") @>

test <@ applies Map.empty (Char 'z') "z" @>
test <@ applies Map.empty (Char 'z') "zab" |> not @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "cab" |> not @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "ab" @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "ba" @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "bac" |> not @>

printfn "..done!"