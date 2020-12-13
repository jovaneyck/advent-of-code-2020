#r @"nuget: Unquote"
open Swensen.Unquote

//HERE BE DRAGONS !!!
//SPOILERS TO 13.2 BELOW

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let remainder idx busid =  (busid - (int64 idx % busid)) % busid

let parse (text : string) =
    let busids = 
        text 
        |> fun s -> s.Split([|','|]) 
        |> Seq.mapi (fun idx bus -> if bus = "x" then None else Some (idx, int64 bus))
        |> Seq.choose id
        |> Seq.map (fun (idx, busid) -> (remainder idx busid, busid))
    busids

//Chinese remainder theorem, fast version (tm)
////https://www.youtube.com/watch?v=zIFehsBHB8o
let inverse Ni divisor = 
    [1L..divisor]
    |> Seq.find (fun d -> (Ni * d) % divisor = 1L)

let solve input =
    let buses = parse input
    let N = buses |> Seq.map snd |> Seq.reduce (*)
    buses
        |> Seq.map (fun (bi, divi) ->
            let Ni = N / divi
            let xi = inverse Ni divi
            let biNixi = bi * Ni * xi
            biNixi)
        |> Seq.sum
        |> (fun sum -> sum % N)

printf "Test.."
//#time "on"
test <@ solve "17,x,13,19" = 3_417L @>
test <@ solve "67,7,59,61" = 754_018L @>
test <@ solve "67,x,7,59,61" = 779210L @>
test <@ solve "67,7,x,59,61" = 1261476L @>
test <@ solve "1789,37,47,1889" = 1_202_161_486L @> 
printfn "done!"

let FINALLY = solve (input |> Seq.item 1)
printfn $"{FINALLY}"
