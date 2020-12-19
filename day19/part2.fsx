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

let (|CharRule|_|) line =
    let m = Regex("^(\d*): \"(.)\"$").Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rule = m.Groups.[2].Value |> char
        Some (ruleNumber, rule)
    else
        None
   
let (|SeqRule|_|) line =
    let m = Regex("^(\d*): ((\d+)\s?)+$").Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let rules = [for capt in m.Groups.[2].Captures -> capt.Value |> int]
        Some (ruleNumber, rules)
    else
        None 

let (|AlternativeRule|_|) line =
    let m = Regex("^(\d*): (.*) \| (.*)$").Match(line)
    if m.Success 
    then 
        let ruleNumber = m.Groups.[1].Value |> int
        let left = m.Groups.[2].Value.Split([|' '|]) |> Seq.map int |> Seq.toList
        let right = m.Groups.[3].Value.Split([|' '|]) |> Seq.map int |> Seq.toList
        Some (ruleNumber, [ left; right ])
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
type ParsingProblem = ParsingProblem of string
let rec parses (rulebook : Rulebook) rule (message : string) : Result<string,ParsingProblem> =
    printfn $"running rule <{rule}> on <{message}>"
    let result = 
        match rule with
        | Char c -> 
            if message.StartsWith(string c)
            then message.Substring(1) |> Ok
            else (sprintf "<%s> does not start with <%c>" message c) |> ParsingProblem |> Error
        | Sequence rules ->
            rules 
            |> Seq.map (fun r -> rulebook |> Map.find r) 
            |> Seq.fold 
                (fun result rule -> result |> Result.bind (fun remainder -> parses rulebook rule remainder))
                (Ok message)
        | Alternative alternatives ->
            let [first;second] = alternatives |> List.map (fun s -> Sequence s)
            let firstResult = parses rulebook first message
            //let secondResult = parses rulebook first message
            //BUG: if first parses, we never check whether option 2 parses as well, which might be the problem!!!
            match firstResult with
            | Ok remainder -> Ok remainder
            | Error (ParsingProblem errf) -> 
                match parses rulebook second message with
                | Ok remainder -> Ok remainder
                | Error (ParsingProblem errs) -> Error <| ParsingProblem (sprintf "both alternatives had problems: %A; %A" errf errs)
    printfn "%A when applying rule %A on <%s>" result rule message
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
updatedRulebook |> Seq.iter (printfn "%A")
//let valid =
//    messages
//    |> Seq.filter (applies updatedRulebook rule0)
//    |> Seq.toList
//valid |> Seq.contains "aaaaabbaabaaaaababaa"
//|> Seq.length

parses updatedRulebook rule0 "aaaaabbaabaaaaababaa"

printf "Test.."
test <@ parseRule @"4: ""a""" = (4, Char 'a') @>
test <@ parseRule "125: 100 116" = (125, Sequence [100; 116]) @>
test <@ parseRule "126: 127 41 | 116 112" = (126, Alternative [[127; 41]; [116; 112]]) @>
test <@ parseRule "13: 116 | 127" = (13, Alternative [[116]; [127]]) @>

test <@ parses Map.empty (Char 'z') "z" = Ok "" @>
test <@ parses Map.empty (Char 'a') "z" = (Error <| ParsingProblem "z does not start with a") @>
test <@ parses Map.empty (Char 'a') "az" = Ok "z" @>

test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "ab" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "abc" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Sequence [1;2]) "cab" = (Error <| ParsingProblem "cab does not start with a") @>

test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "a" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "b" = (Ok "") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "c" = (Error <| ParsingProblem @"both alternatives had problems: ""c does not start with a""; ""c does not start with b""") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1];[2]]) "ab" = (Ok "b") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "abc" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "bac" = (Ok "c") @>
test <@ parses ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "cab" = (Error <| ParsingProblem @"both alternatives had problems: ""cab does not start with a""; ""cab does not start with b""") @>

test <@ applies Map.empty (Char 'z') "z" @>
test <@ applies Map.empty (Char 'z') "zab" |> not @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "cab" |> not @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "ab" @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "ba" @>
test <@ applies ([(1, Char 'a');(2, Char 'b')] |> Map.ofSeq) (Alternative [[1;2];[2;1]]) "bac" |> not @>

printfn "..done!"