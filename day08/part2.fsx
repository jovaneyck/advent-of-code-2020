#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let parse (text : char list) =
    match text with
    | 'n' :: 'o' :: 'p' :: arg -> arg |> Seq.map string |>  String.concat "" |> int |> Nop
    | 'a' :: 'c' :: 'c' :: arg -> arg |> Seq.map string |>  String.concat "" |> int |> Acc
    | 'j' :: 'm' :: 'p' :: arg -> arg |> Seq.map string |>  String.concat "" |> int |> Jmp
    | i -> failwith $"unknown instruction: {i}"

type State = { instructionPointer : int; accumulator : int; visitedInstructionPointers : int Set }
let init = { instructionPointer = 0; accumulator = 0; visitedInstructionPointers = Set.empty}

let runNext ( instructions : Instruction list) state =
    let next = instructions.[state.instructionPointer]
    match next with
    | Nop _ -> 
        { state with 
            instructionPointer = state.instructionPointer + 1
            visitedInstructionPointers = state.visitedInstructionPointers |> Set.add state.instructionPointer
        }
    | Jmp j -> 
        { state with 
            instructionPointer = state.instructionPointer + j
            visitedInstructionPointers = state.visitedInstructionPointers |> Set.add state.instructionPointer
        }
    | Acc a -> 
        { state with 
            instructionPointer = state.instructionPointer + 1
            accumulator = state.accumulator + a
            visitedInstructionPointers = state.visitedInstructionPointers |> Set.add state.instructionPointer
        }

let rec runProgram instructions state : State option =
    if state.instructionPointer >= Seq.length instructions then
        Some state
    else if state.visitedInstructionPointers |> Set.contains state.instructionPointer then
        None
    else
        let nextState = runNext instructions state
        runProgram instructions nextState

let rec variants =
    function
    | [] -> Seq.singleton Seq.empty
    | Acc a :: prog -> variants prog |> Seq.map (Seq.append (Seq.singleton <| Acc a))
    | Jmp j :: prog -> 
        let nonFlipped = variants prog |> Seq.map (Seq.append (Seq.singleton <| Jmp j))
        let flipped = (Nop j :: prog) |> Seq.ofList |> Seq.singleton 
        Seq.append nonFlipped flipped
    | Nop n :: prog -> 
        let nonFlipped = variants prog |> Seq.map (Seq.append (Seq.singleton <| Nop n))
        let flipped = (Jmp n :: prog) |> Seq.ofList |> Seq.singleton 
        Seq.append nonFlipped flipped

let parseProgram = Seq.map (Seq.toList >> parse) >> List.ofSeq

let instructions = input |> parseProgram
let allVariants = variants instructions
let part2 = allVariants |> Seq.choose (fun variant -> runProgram (variant |> List.ofSeq) init) |> Seq.head

printf "Test.."
test <@ example |> parseProgram |> variants |> Seq.choose (fun variant -> runProgram (variant |> List.ofSeq) init) |> Seq.head |> (fun s -> s.accumulator) = 8 @>
printfn "done!"