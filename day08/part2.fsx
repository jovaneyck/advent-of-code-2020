#r @"nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"
let example = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\example.txt"

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let parse (text : string) =
    match text with
    | i when i.StartsWith("nop") -> i.Substring 4 |> int |> Nop
    | i when i.StartsWith("acc") -> i.Substring 4 |> int |> Acc
    | i when i.StartsWith("jmp") -> i.Substring 4 |> int |> Jmp
    | i -> failwith $"unknown instruction: {i}"

type State = { instructionPointer : int; accumulator : int; visitedInstructionPointers : int Set }
let init = { instructionPointer = 0; accumulator = 0; visitedInstructionPointers = Set.empty}

let runNext ( instructions : Instruction list) state =
    let next = instructions.[state.instructionPointer]
    let visited = { state with visitedInstructionPointers = state.visitedInstructionPointers |> Set.add state.instructionPointer }
    match next with
    | Nop _ -> 
        { visited with 
            instructionPointer = state.instructionPointer + 1 }
    | Jmp j -> 
        { visited with 
            instructionPointer = state.instructionPointer + j }
    | Acc a -> 
        { visited with 
            instructionPointer = state.instructionPointer + 1
            accumulator = state.accumulator + a }

let rec runProgram state instructions : State option =
    if state.instructionPointer >= Seq.length instructions then
        Some state
    else if state.visitedInstructionPointers |> Set.contains state.instructionPointer then
        None
    else
        let nextState = runNext instructions state
        runProgram nextState instructions

let rec variants : Instruction list -> Instruction list seq =
    function
    | [] -> Seq.singleton List.empty
    | Acc a :: prog -> 
        variants prog |> Seq.map (List.append [Acc a])
    | Jmp j :: prog -> 
        let flipped = [Nop j :: prog]
        let nonFlipped = variants prog |> Seq.map (List.append [Jmp j])
        Seq.append flipped nonFlipped
    | Nop n :: prog -> 
        let flipped = [Jmp n :: prog]
        let nonFlipped = variants prog |> Seq.map (List.append [Nop n])
        Seq.append flipped nonFlipped

let parseProgram = Seq.map parse >> List.ofSeq
let run input =
    input
    |> parseProgram
    |> variants
    |> Seq.pick (runProgram init)
    |> (fun s -> s.accumulator)

let part2 = run input

printf "Test.."
test <@ run example = 8 @>
printfn "done!"