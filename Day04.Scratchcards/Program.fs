open System
open System.IO
open System.Text.RegularExpressions

type ScratchCard = { Id: int; WinningNumbers: int array; CardNumbers: int array }

let rec ScratchCardPointValue (card: ScratchCard) =
    Set.intersect (card.CardNumbers |> Set.ofArray) (card.WinningNumbers |> Set.ofArray)
    |> Set.count
    |> fun f ->
        match f with
        | 0 -> 0
        | n -> Math.Pow(2, (n - 1) |> float) |> int


let rec ParseLine line =
    match Regex.Match(line, @"(Card\s+(?<card_id>\d+)): (?<numbers>.*)") with
    | m when m.Success ->
        { Id = m.Groups["card_id"].Value |> int
          WinningNumbers =
            Regex.Matches(m.Groups["numbers"].Value.Split("|")[0], @"\d+")
            |> Seq.map (fun f -> f.Value |> int)
            |> Seq.toArray
          CardNumbers =
            Regex.Matches(m.Groups["numbers"].Value.Split("|")[1], @"\d+")
            |> Seq.map (fun f -> f.Value |> int)
            |> Seq.toArray }
    | _ -> failwithf $"Could not parse input line %s{line}"

let ParseFile file = file |> File.ReadAllLines |> Seq.map ParseLine

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let cards = ParseFile file
    let pointsPerCard = cards |> Seq.map ScratchCardPointValue
    let totalPointsForCards = pointsPerCard |> Seq.sum

    printfn $"[*]: Total point worth of all cards: %i{totalPointsForCards}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
