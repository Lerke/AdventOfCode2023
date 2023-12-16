open System
open System.IO

type History = { NumberSequence: HistoryElement array }

and HistoryElement =
    | Number of int
    | Placeholder

let rec ExtrapolateSequence (sequence: HistoryElement array) (sequences: HistoryElement array list) =
    let newSequence =
        sequence
        |> Seq.pairwise
        |> Seq.map (fun (a, b) ->
            match (a, b) with
            | (Number a, Number b) -> Number(b - a)
            | _ -> failwithf "Parse error")
        |> Seq.toArray

    match (newSequence |> Seq.forall (fun f -> f = Number 0)) with
    | true -> sequences @ [ newSequence ]
    | _ -> ExtrapolateSequence newSequence (sequences @ [ newSequence ])

let AddPlaceholders (sequences: HistoryElement array list) =
    sequences
    |> List.rev
    |> List.skip 1
    |> List.map (fun f -> Array.append f [| Placeholder |])
    |> List.append [ sequences |> List.last |> Array.append [| Number 0 |] ]
    |> List.rev

let SolvePlaceholders (sequences: HistoryElement array list) =
    sequences
    |> List.rev
    |> List.fold
        (fun acc ele ->
            acc
            @ [ ele
                |> Array.mapi (fun idx e ->
                    match e with
                    | Number n -> Number n
                    | Placeholder ->
                        match (((acc |> List.last) |> Array.last), ele.[idx - 1]) with
                        | (Number p, Number q) -> (Number(p + q))
                        | _ -> failwithf "unable to parse") ])
        []
    |> List.rev

let ParseLine (line: string) =
    line.Split(" ")
    |> fun f -> { NumberSequence = f |> Seq.map (fun f -> Number(f |> int)) |> Seq.toArray }

let ParseInput file = file |> File.ReadAllLines |> Seq.map ParseLine

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let histories = ParseInput file

    let extrapolatedHistories =
        histories
        |> Seq.map (fun f -> ExtrapolateSequence f.NumberSequence [ f.NumberSequence ])
        |> Seq.map AddPlaceholders
        |> Seq.map SolvePlaceholders
        |> Seq.toList

    let finalValue =
        extrapolatedHistories
        |> List.map (fun f ->
            match (f.[0] |> Array.last) with
            | Number n -> n
            | Placeholder -> failwith "Parse error")
        |> List.sum

    printfn $"[*] Sum of extrapolated values {finalValue}"

    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
