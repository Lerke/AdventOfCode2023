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

let AddPlaceholders mappingFun (sequences: HistoryElement array list) =
    sequences
    |> List.rev
    |> List.skip 1
    |> List.map (fun f -> mappingFun f [| Placeholder |])
    |> List.append [ sequences |> List.last |> (fun f -> mappingFun f [| Number 0 |]) ]
    |> List.rev

let SolvePlaceholders (sequences: HistoryElement array list) selector additionFn =
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
                        match (selector acc ele idx) with
                        | (Number p, Number q) -> (Number(additionFn q p))
                        | _ -> failwithf "unable to parse") ])
        []
    |> List.rev

let ParseLine (line: string) =
    line.Split(" ")
    |> fun f -> { NumberSequence = f |> Seq.map (fun f -> Number(f |> int)) |> Seq.toArray }

let ParseInput file = file |> File.ReadAllLines |> Seq.map ParseLine

let FinalValue (values: HistoryElement array list list) selector =
    values
    |> List.map (fun f ->
        match (f.[0] |> selector) with
        | Number n -> n
        | Placeholder -> failwith "Parse error")
    |> List.sum

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let histories = ParseInput file

    let extrapolatedHistories =
        histories
        |> Seq.map (fun f -> ExtrapolateSequence f.NumberSequence [ f.NumberSequence ])
        |> Seq.map (AddPlaceholders Array.append)
        |> Seq.map (fun f -> SolvePlaceholders f (fun acc ele idx -> ((acc |> List.last) |> Array.last), ele.[idx - 1]) (+))
        |> Seq.toList

    let finalValue = FinalValue extrapolatedHistories Array.last
    printfn $"[*] Sum of extrapolated values {finalValue}"

    let beginningExtrapolatedHistories =
        histories
        |> Seq.map (fun f -> ExtrapolateSequence f.NumberSequence [ f.NumberSequence ])
        |> Seq.map (AddPlaceholders(fun f p -> Array.append p f))
        |> Seq.map (fun f -> SolvePlaceholders f (fun acc ele _ -> ((acc |> List.last) |> Array.head), ele.[1]) (-))
        |> Seq.toList

    let finalPartTwo = FinalValue beginningExtrapolatedHistories Array.head
    printfn $"[**] Sum of extrapolated values {finalPartTwo}"

    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
