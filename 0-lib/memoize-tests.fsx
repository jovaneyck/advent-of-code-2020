#r "nuget: Unquote"
#load "Memo.fsx"
open Swensen.Unquote

let rec slowFib n =
    match n with
    | 0 -> 0L
    | 1 -> 1L
    | n -> slowFib (n - 1) + slowFib (n - 2)

let fib r n =
    match n with
    | 0 -> 0L
    | 1 -> 1L
    | n -> r (n - 1) + r (n - 2)
let mfib = Memo.memoize fib

#time
test <@ slowFib 40 = 102334155L @>  //Real: 00:00:02.313, CPU: 00:00:01.750, GC gen0: 0, gen1: 0, gen2: 0
test <@ mfib 40 = 102334155L @>     //Real: 00:00:00.002, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
