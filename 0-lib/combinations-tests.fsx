#r "nuget: Unquote"
#load "Combinations.fsx"
open Swensen.Unquote

printf "Testing..."
test <@ Combinations.combinations 1 [1..3] = [[1];[2];[3]] @>
test <@ Combinations.combinations 2 [1..3] = [[1; 2]; [1; 3]; [2; 3]] @>
test <@ Combinations.combinations 3 [1..3] = [[1; 2; 3]] @>
test <@ Combinations.combinations 3 [1..4] = [[1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]] @>
printfn "...Done!"