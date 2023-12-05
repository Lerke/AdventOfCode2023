open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type ScratchCard =
    { Id: int
      WinningNumbers: int array
      CardNumbers: int array }


let ScratchCardMatchingNumbers (card: ScratchCard) =
    Set.intersect (card.CardNumbers |> Set.ofArray) (card.WinningNumbers |> Set.ofArray)
    |> Set.count

let rec ScratchCardPointValue (card: ScratchCard) =
    card
    |> ScratchCardMatchingNumbers
    |> fun f ->
        match f with
        | 0 -> 0
        | n -> Math.Pow(2, (n - 1) |> float) |> int

let rec SolvePartTwo (cardCounts: Map<int, (int * ScratchCard)>) (index: int) =
    match cardCounts.ContainsKey index with
    | false -> cardCounts
    | true ->
        let currentCard = cardCounts[index]
        let mutable cards = cardCounts
        let currentCardValue = ScratchCardMatchingNumbers(snd currentCard)
        let multiplier = (fst currentCard)

        match (currentCardValue > 0) with
        | true ->
            [| (index + 1) .. (index + currentCardValue) |]
            |> Array.iter (fun f ->
                let el = cardCounts[f]
                cards <- cards |> Map.remove f |> Map.add f (((fst el) + multiplier), snd el))

            SolvePartTwo cards (index + 1)
        | false -> SolvePartTwo cards (index + 1)

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

let ParseFile file =
    file |> File.ReadAllLines |> Seq.map ParseLine

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let cards = ParseFile file
    let pointsPerCard = cards |> Seq.map ScratchCardPointValue
    let totalPointsForCards = pointsPerCard |> Seq.sum

    printfn $"[*]: Total point worth of all cards: %i{totalPointsForCards}"

    let groupedCards =
        cards
        |> Seq.groupBy (fun f -> f.Id)
        |> Seq.map (fun f -> ((fst f), (1, (snd f |> Seq.head))))
        |> Map

    let totalCards = SolvePartTwo groupedCards 1
    let totalCardCount = totalCards.Values |> Seq.sumBy fst

    printfn $"[**]: Total cards %i{totalCardCount}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
