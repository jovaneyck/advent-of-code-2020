open System.Collections.Generic

let memoize f =
  let mem = Dictionary<'a, 'b>();
  let rec g key = h g key
  and h r key =
    match mem.TryGetValue(key) with
    | (true, value) ->
        value
    | _ ->
        let value = f g key
        mem.Add(key, value)
        value
  g