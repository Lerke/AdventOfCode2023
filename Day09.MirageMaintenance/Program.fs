open System
open System.IO

type History = { NumberSequence: int seq }

let rec ExtrapolateSequence (sequence: int seq) (sequences: int seq list) =
    let newSequence =
        sequence |> Seq.pairwise |> Seq.map (fun (a, b) -> abs (b - a)) |> Seq.toList

    match (newSequence |> Seq.forall (fun f -> f = 0)) with
    | true -> sequences @ [ newSequence ]
    | _ -> ExtrapolateSequence newSequence (sequences @ [ newSequence ])

let AddPlaceholders (sequences: int seq list) = sequences |> List.map (fun f -> f |> Seq.append (seq { 0 }))

let ParseLine (line: string) = line.Split(" ") |> fun f -> { NumberSequence = f |> Seq.map int }

let ParseInput file = file |> File.ReadAllLines |> Seq.map ParseLine

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let history = ParseInput file |> Seq.toList

    let p =
        ExtrapolateSequence (history.Head).NumberSequence [ (history.Head).NumberSequence ]

    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
