#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"  

type Rule =
    | Char of char
    | Sequence of int list
    | Alternative of int list list
type Rulebook = Map<int,Rule>
let charRegex = Regex("(\d*): \"(.)\"")
let (|CharRule|_|) line =
    let m = charRegex.Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rule = m.Groups.[2].Value |> char
        Some (ruleNumber, rule)
    else
        None
   
let sequenceRegex = Regex("(\d*): ((\d+)\s?)+")
let (|SeqRule|_|) line =
    let m = sequenceRegex.Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rules = [for capt in m.Groups.[2].Captures -> capt.Value |> int]
        Some (ruleNumber, rules)
    else
        None 

let choiceRegex = Regex("(\d*): (\d*) (\d*) \| (\d*) (\d*)")
let (|ChoiceRule|_|) line =
    let m = choiceRegex.Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rules = [ [ m.Groups.[2].Value |> int; m.Groups.[3].Value |> int ]
                      [ m.Groups.[4].Value |> int; m.Groups.[5].Value |> int ] ]
        Some (ruleNumber, rules)
     else
         None 

let parseRule line =
    match line with
    | CharRule (nb, c) -> (nb, Char c)
    | ChoiceRule (nb, r) -> (nb, Alternative r)
    | SeqRule (nb, r) -> (nb, Sequence r)
    
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

let applies rulebook rule message = 
    match parses rulebook rule message with
    | Ok "" -> true
    | _ -> false

let (rulebook, messages) = parse input
let rule0 = rulebook |> Map.find 0
messages
|> Seq.filter (applies rulebook rule0)
|> Seq.length

printf "Test.."
test <@ parseRule @"4: ""a""" = (4, Char 'a') @>
test <@ parseRule "125: 100 116" = (125, Sequence [100; 116]) @>
test <@ parseRule "126: 127 41 | 116 112" = (126, Alternative [[127; 41]; [116; 112]]) @>

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