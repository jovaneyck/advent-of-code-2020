#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

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

let instructions = input |> Seq.map (Seq.toList >> parse) |> List.ofSeq

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

let rec runProgram instructions state : State =
    if state.visitedInstructionPointers |> Set.contains state.instructionPointer then
        state
    else
        let nextState = runNext instructions state
        runProgram instructions nextState

let result = runProgram instructions init
result.visitedInstructionPointers |> Seq.length

printf "Test.."
printfn "done!"