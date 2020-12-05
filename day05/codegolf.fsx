open System
let input = IO.File.ReadAllLines $@"{__SOURCE_DIRECTORY__}\input.txt"
  
let ids = set (Seq.map (Seq.map (function | 'F' | 'L' -> "0" | 'B' | 'R' -> "1" ) >> String.concat "" >> (fun n -> Convert.ToInt32(n, 2))) input)
Set.difference (set [ ids |> Seq.min .. ids |> Seq.max ]) ids