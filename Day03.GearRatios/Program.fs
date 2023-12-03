open System
open System.IO

type EngineSchematicElement =
    | Number of int
    | Symbol of char
    | Nothing

type EngineSchematic = EngineSchematicElement[,]


let SurroundingTiles (schematic: EngineSchematic) x y =
    let xyMax = schematic[0, *].Length

    [ (y - 1, x - 1)
      (y - 1, x)
      (y - 1, x + 1)
      (y, x - 1)
      (y, x + 1)
      (y + 1, x - 1)
      (y + 1, x)
      (y + 1, x + 1) ]
    |> List.filter (fun (y, x) -> y >= 0 && x >= 0 && y < xyMax && x < xyMax)

let SymbolIndices (schematic: EngineSchematic) =
    schematic
    |> Array2D.mapi (fun i j v ->
        match v with
        | Symbol(c') -> Some((j, i), c')
        | _ -> None)
    |> Seq.cast<Option<(int * int) * char>>
    |> Seq.filter (fun f -> f.IsSome)
    |> Seq.map (fun f -> f.Value)
    |> Seq.toArray
    |> Map.ofArray

let NumberAtPosition (schematic: EngineSchematic) x y =
    match schematic[*, 0].Length > y with
    | false -> None
    | true ->
        match schematic[0, *].Length > x with
        | false -> None
        | true ->
            match schematic[y, x] with
            | Number _ ->
                let before =
                    schematic[y, .. x - 1]
                    |> Seq.rev
                    |> Seq.takeWhile (function
                        | Number _ -> true
                        | _ -> false)
                    |> Seq.rev
                    |> Seq.toList

                let after =
                    schematic[y, x + 1 ..]
                    |> Seq.takeWhile (function
                        | Number _ -> true
                        | _ -> false)
                    |> Seq.toList

                let parsedNumber =
                    (before @ [ schematic[y, x] ] @ after)
                    |> List.map (function
                        | Number(f') -> f')
                    |> List.fold (fun acc elem -> acc + elem.ToString()) ""

                Some((x - before.Length, x + after.Length, y), parsedNumber |> int)
            | _ -> None

let SurroundingNumbersAtPosition (schematic: EngineSchematic) x y =
    let surrounding = SurroundingTiles schematic x y

    surrounding
    |> List.map (fun (y, x) -> (NumberAtPosition schematic y x))
    |> List.filter (fun f -> f.IsSome)
    |> List.map (fun f -> f.Value)

let ParseLine (line: string) =
    let z = ref 0

    line
    |> Seq.map (fun f ->
        match f with
        | x when Int32.TryParse(x |> string, z) = true -> Number z.Value
        | x when x <> '.' -> Symbol x
        | _ -> Nothing)

let ParseFile file =
    file |> File.ReadAllLines |> Seq.map ParseLine |> array2D

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let parsed = ParseFile file

    let symbols = SymbolIndices parsed

    let sum =
        (symbols).Keys
        |> Seq.map (fun (y, x) -> SurroundingNumbersAtPosition parsed x y)
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.sumBy snd

    printfn $"[*] Sum of all part numbers in schematic: %i{sum}"

    let gearSymbols = symbols |> Map.filter (fun _ s -> s = '*')

    let gearRatios =
        gearSymbols.Keys
        |> Seq.map (fun (y, x) -> SurroundingNumbersAtPosition parsed x y)
        |> Seq.map List.distinct
        |> Seq.filter (fun f -> f.Length = 2)
        |> Seq.distinct
        |> Seq.collect id
        |> Seq.map snd
        |> Seq.chunkBySize 2
        |> Seq.sumBy (fun f -> f[0] * f[1])

    printfn $"[**] Gear ratios in engine schematic: %i{gearRatios}"

    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
