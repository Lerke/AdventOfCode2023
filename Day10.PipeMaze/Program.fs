open System
open System.IO

type Map = { Tiles: Tile[,]; StartingPosition: int * int }

and Tile =
    | VerticalPipe
    | HorizontalPipe
    | NorthEastBend
    | NorthWestBend
    | SouthWestBend
    | SouthEastBend
    | Ground
    | StartingPosition

let ParseTile (tile: char) =
    match tile with
    | '|' -> VerticalPipe
    | '-' -> HorizontalPipe
    | 'L' -> NorthEastBend
    | 'J' -> NorthWestBend
    | '7' -> SouthWestBend
    | 'F' -> SouthEastBend
    | '.' -> Ground
    | 'S' -> StartingPosition
    | n -> failwithf $"Unknown input: {n}"

let NextTile (x: int) (y: int) (px: int) (py: int) tile =
    match tile with
    | VerticalPipe -> if py < y then (x, y + 1) else (x, y - 1)
    | HorizontalPipe -> if (px < x) then (x + 1, y) else (x - 1, y)
    | NorthEastBend -> if (py < y) then (x + 1, y) else (x, y - 1)
    | NorthWestBend -> if (py < y) then (x - 1, y) else (x, y - 1)
    | SouthWestBend -> if (py > y) then (x - 1, y) else (x, y + 1)
    | SouthEastBend -> if (py > y) then (x + 1, y) else (x, y + 1)
    | Ground -> failwith "we should never navigate over ground!"
    | StartingPosition -> failwith "Starting position should not be part of main loop!"

let SelectTile x y (tiles: Tile[,]) =
    if y < 0 || y >= tiles.[*, 0].Length then Ground
    elif x < 0 || x >= tiles.[y, *].Length then Ground
    else tiles.[y, x]

let ConnectingPositions (x: int) (y: int) tiles =
    match (SelectTile x y tiles) with
    | VerticalPipe -> x, y - 1
    | HorizontalPipe -> (x + 1), y
    | NorthEastBend -> x, y - 1
    | NorthWestBend -> x, y - 1
    | SouthWestBend -> x, y + 1
    | SouthEastBend -> x, y + 1
    | _ -> failwithf "This tile has no connecting positions"

let rec FollowPipe (map: Map) (position: int * int) (nextPosition: int * int) (visited: (int * int) list) =
    printf $"{snd position}, {fst position}"
    match (visited |> List.contains position) with
    | true -> visited
    | false ->
        let py, px = position
        let y, x = nextPosition
        printf " -> "
        let nextTile = NextTile x y px py (SelectTile x y map.Tiles)
        printfn $"{x}, {y}"
        FollowPipe map (y, x) nextTile (visited @ [ position ])

let ParseMapFile file =
    let rec startPosition x y (tiles: Tile[,]) =
        if x >= tiles[0,*].Length then startPosition 0 (y + 1) tiles
        elif tiles.[y, x] = StartingPosition then (x, y)
        else startPosition (x + 1) y tiles

    let determineStartingTile x y tiles =
        let leftTile = SelectTile (x - 1) y tiles
        let rightTile = SelectTile (x + 1) y tiles
        let topTile = SelectTile x (y - 1) tiles
        let bottomTile = SelectTile x (y + 1) tiles

        let topConnects =
            [| VerticalPipe; SouthEastBend; SouthWestBend |] |> Array.contains topTile

        let bottomConnects =
            [| VerticalPipe; NorthEastBend; NorthWestBend |] |> Array.contains bottomTile

        let rightConnects =
            [| HorizontalPipe; SouthWestBend; NorthWestBend |] |> Array.contains rightTile

        let leftConnects =
            [| HorizontalPipe; NorthEastBend; SouthEastBend |] |> Array.contains leftTile

        match (topConnects, bottomConnects, rightConnects, leftConnects) with
        | true, true, _, _ -> VerticalPipe
        | true, _, true, _ -> NorthEastBend
        | true, _, _, true -> NorthWestBend
        | _, true, true, _ -> SouthEastBend
        | _, true, _, true -> SouthWestBend
        | _, _, true, true -> HorizontalPipe
        | _ -> failwith "Failed to determine starting tile"

    let tiles =
        file |> File.ReadLines |> Seq.map (fun f -> f |> Seq.map ParseTile) |> array2D

    let startingPosition = startPosition 0 0 tiles

    let startingTile =
        determineStartingTile (fst startingPosition) (snd startingPosition) tiles

    tiles[(fst startingPosition), (snd startingPosition)] <- startingTile

    let map = { Tiles = tiles; StartingPosition = startingPosition }
    map

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let map = ParseMapFile file

    let nextPosition =
        ConnectingPositions (fst map.StartingPosition) (snd map.StartingPosition) map.Tiles

    let path = FollowPipe map map.StartingPosition nextPosition []

    let pathLengths =
        path |> List.mapi (fun i f -> {| Tile = f; Steps = (min i (path.Length - i)) |})

    let furthestPoint = pathLengths |> List.maxBy (_.Steps)

    printfn $"[*] Steps along the loop to point farthest from starting position: {furthestPoint.Steps}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
