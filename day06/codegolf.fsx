open System
let input = IO.File.ReadAllText $@"{__SOURCE_DIRECTORY__}\input.txt"
let none = StringSplitOptions.None
let groups = input.Split([|"\r\n\r\n"|], none)
groups 
|> Seq.map (fun g -> 
    g.Split([|"\r\n"|], none) 
    |> Seq.map Set.ofSeq 
    |> Set.intersectMany ) 
|> Seq.sumBy Seq.length