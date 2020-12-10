let combinations (n : int) (xs : 'a list) = 
    let idx = xs |> List.indexed

    let combinations1  =
        [for (i,a) in idx -> [a]]
    let combinations2 =
        [for (i,a) in idx do
            for (j,b) in idx.[i+1..] -> [a;b]]     
    let combinations3 =
        [for (i,a) in idx do
            for (j,b) in idx.[i+1..] do
                for(k,c) in idx.[j+1..] -> [a;b;c]]   
    match n with
    | 1 -> combinations1
    | 2 -> combinations2   
    | 3 -> combinations3
    | _ -> failwith "not supported mkay"