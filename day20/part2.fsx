﻿#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions


let monsterpattern = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\monster-pattern.txt"
//let example = System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\example.txt"  
let input = System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"  

type Tile = { ID : int; contents : char[,] }
type TileConfigurations = { ID : int; configurations : char[,] list }

let m = array2D [[1;2;3];[4;5;6];[7;8;9]]

let rot90 m =
    [for col in (Array2D.length1 m) - 1 .. -1 .. 0 -> 
        m.[0..,col]
    ]
    |> array2D

let flipHorizontal m =
    [for row in (Array2D.length1 m) - 1 .. -1 .. 0 -> 
        m.[row,0..]
    ]
    |> array2D  

let trimBorders (m : 'a [,]) =
    m.[1..(Array2D.length1 m - 2),1..(Array2D.length1 m - 2)]

let merge (nested : char [,][,]) : char[,] =
    let outerDim = Array2D.length1 nested
    let innerDim = Array2D.length1 nested.[0,0]
    [for outer in 0..outerDim - 1 do
        for inner in 0..innerDim - 1 ->
            nested.[outer,0..] |> Seq.collect (fun m -> m.[inner,0..]) |> Seq.toList]
    |> array2D

let parse (input : string) = 
    let tileIDRegex = Regex("Tile (\d*):")
    let parseTile (text :string) =
        let lines = text.Split([|"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
        let id = (tileIDRegex.Match(lines.[0])).Groups.[1].Value |> int
        let contents = array2D lines.[1..]
        { ID = id; contents = contents }

    input.Split([|"\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseTile
    |> Seq.toList
let topBorder (tile : 'a [,]) = tile.[0,0..]
let leftBorder (tile : 'a [,]) = tile.[0..,0]
let bottomBorder (tile : 'a [,]) = tile.[(Array2D.length1 tile - 1),0..]
let rightBorder (tile : 'a [,]) = tile.[0..,(Array2D.length1 tile - 1)]
let borders (tile : 'a [,]) =
    [ 
        topBorder tile  
        bottomBorder tile
        leftBorder tile
        rightBorder tile
    ]

let configurations tile = 
    [tile
     tile |> rot90
     tile |> rot90 |> rot90
     tile |> rot90 |> rot90 |> rot90]
    |> List.collect (fun t -> [t; flipHorizontal t])

let findCorners configs tiles =
    let nbUnmatchedBorders tile (configs : TileConfigurations list) = 
        let bords = borders tile.contents
        let nbMatchesPerBorder =
            bords
            |> Seq.map (fun border -> 
                configs 
                |> Seq.filter (fun config -> 
                    config.configurations 
                    |> List.exists (fun c -> c |> borders |> List.contains border)) 
                |> Seq.length)
        nbMatchesPerBorder |> Seq.filter ((=) 0) |> Seq.length
    
    tiles
    |> List.filter (fun t -> nbUnmatchedBorders t (configs |> List.filter (fun c ->  c.ID <> t.ID)) = 2)

let configurationsTile (tile : Tile) = 
    { ID = tile.ID; configurations = configurations tile.contents}

let part1 input =
    let tiles = parse input
    let configs = tiles |> List.map configurationsTile
    let corners = findCorners configs tiles

    corners |> Seq.map (fun t -> int64 t.ID) |> Seq.reduce (*)

let findPiecesThatMatch border (tiles : TileConfigurations seq) : Tile seq =
    tiles
    |> Seq.choose (fun t -> 
            t.configurations 
            |> List.tryFind (fun c -> c |> borders |> Seq.contains border)
            |> Option.map (fun cfg -> { ID = t.ID; contents = cfg}))

//let's start with a random corner:
let tiles = parse input
let configs = tiles |> List.map (fun t -> t.ID, configurationsTile t) |> Map.ofSeq
let corners = findCorners (configs |> Map.toList |> List.map snd) tiles
let topLeft = corners.[0]
//how do we orient topleft? with a border that has matches down + right

let topleftConfigs = configs |> Map.find topLeft.ID
let otherConfigs = configs |> Map.remove topLeft.ID

let orientedTopLeft = 
    topleftConfigs.configurations
    //TODO: we have more than 1 valid configuration here; do we need to look at them all??
    |> List.find (fun cfg ->
        let right = cfg |> rightBorder
        let bottom = cfg |> bottomBorder
        (findPiecesThatMatch right (otherConfigs |> Map.toSeq |> Seq.map snd) |> Seq.length > 0) 
        && (findPiecesThatMatch bottom (otherConfigs |> Map.toSeq |> Seq.map snd) |> Seq.length > 0))
    |> (fun cfg -> { ID = topleftConfigs.ID; contents = cfg})

//Sooooo now we have our top left puzzle piece and rotated/flipped it as desired. Let's start building the puzzle!
let size = sqrt (tiles |> Seq.length |> float) |> int
//We need to fill a size x size grid and already figured out topleft
//Let's go left-to-right,top-to-bottom and try to match a next piece by looking at the border above and to the left of it!
let puzzle = [(0,0), orientedTopLeft] |> Map.ofSeq
let tasks = [for y in 0..(size-1) do for x in 0 .. (size - 1) -> (x,y)] |> List.except [(0,0)]

let placeNextPiece (puzzle,configurations) (x,y) =
    let above = puzzle |> Map.tryFind (x,y-1) |> Option.map (fun tile -> bottomBorder tile.contents)
    let left = puzzle |> Map.tryFind (x-1,y) |> Option.map (fun tile -> rightBorder tile.contents)
    //find the piece that has above as upper border and left as left border
    let cfgsList = configurations |> Map.toSeq |> Seq.map snd
    let piece =
        match above, left with
        | None, None ->
            failwith "This can't be right, we always have the top left piece placed down so we always need an above and/or left!"
        | Some a, Some l ->
            cfgsList 
            |> Seq.pick (fun tile -> 
                tile.configurations 
                |> Seq.tryFind (fun cfg -> a = topBorder cfg && l = leftBorder cfg) 
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))
        | Some a, None ->
            cfgsList
            |> Seq.pick (fun tile -> 
                tile.configurations 
                |> Seq.tryFind (fun cfg -> a = topBorder cfg )
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))
        | None, Some l ->
            cfgsList 
            |> Seq.pick (fun tile -> 
                tile.configurations 
                |> Seq.tryFind (fun cfg -> l = leftBorder cfg)
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))
    (puzzle |> Map.add (x,y) piece, configurations |> Map.remove piece.ID)

let (completedPuzzle, _) = tasks |> List.fold placeNextPiece (puzzle, otherConfigs)
//Let's trim everything and dump in a single bitmap
let trimmed = 
    [for col in 0..size-1 do
        [for row in 0..size-1 ->
            let trimmedPiece = completedPuzzle |> Map.find (row, col) |> (fun tile -> trimBorders tile.contents)
            trimmedPiece]]
    |> array2D

let merged = merge trimmed

//Now let's hunt for sea monsters!
let AAH_MONSTER =
    [for (y,row) in monsterpattern |> Seq.indexed do
        for (x,char) in row |> Seq.indexed do
            if char = '#' then yield (x,y)]

//Try to find a single monster by looping over all cells and applying the pattern
let dim = merged |> Array2D.length1 //still a square
let locations = [for y in 0..dim-1 do for x in 0..dim-1 -> (x,y)]

let monsterfound (grid : char[,]) (x,y) = 
    let dim = grid |> Array2D.length1
    AAH_MONSTER
    |> Seq.forall (fun (mx, my) -> 
            x + mx < dim && y + my < dim && grid.[x+mx,y+my] = '#')

//TODO: we have 2 monsters for the example, but they are in the wrong spot?

//We need to flip/rotate our end puzzle, only one contains le sea monsters!
let hitsPerConfigurations = 
    configurations merged
    |> List.map (fun merged ->
        locations
        |> List.filter (fun loc -> monsterfound merged loc))

let monsterLocations = hitsPerConfigurations |> List.sortByDescending Seq.length |> Seq.head
let nbMonsters = monsterLocations |> Seq.length //18
let monsterSize = AAH_MONSTER |> Seq.length //15

let totalRoughSpots = 
    [for row in 0 .. dim - 1 do
        for col in 0 .. dim - 1 do
            if merged.[row,col] = '#' then yield 1
    ] |> List.sum

let part2 = totalRoughSpots - (monsterSize * nbMonsters)


printf "Test.."
test <@ array2D [[1;2];[3;4]] |> rot90 = array2D [[|2; 4|]; [|1; 3|]]  @>
test <@ array2D [[1;2];[3;4]] |> rot90 |> rot90 = array2D [[|4; 3|]; [|2; 1|]]  @>
test <@ array2D [[1;2];[3;4]] |> rot90 |> rot90 |> rot90 |> rot90 = array2D [[1;2];[3;4]]  @>
test <@ array2D [[1;2];[3;4]] |> flipHorizontal = array2D [[|3; 4|]; [|1; 2|]]  @>
test <@ array2D [[1;2];[3;4]] |> borders =[[|1; 2|]; [|3; 4|]; [|1; 3|]; [|2; 4|]]  @>
test <@ [[1;2;3];[4;5;6];[7;8;9]] |> array2D |> trimBorders = array2D [[5]] @>
test <@ [   [[['1';'2'];['3';'4']]|>array2D;[['5';'6'];['7';'8']]|>array2D]; 
            [[['a';'b'];['c';'d']]|>array2D;[['e';'f'];['g';'h']]|>array2D]] 
            |> array2D 
            |> merge 
                = ([['1';'2';'5';'6'];['3';'4';'7';'8'];['a';'b';'e';'f'];['c'; 'd'; 'g'; 'h']] |> array2D) @>

//test <@ part1 example = 20899048083289L @>
//test <@ part1 input = 14129524957217L @>
printfn "..done!"