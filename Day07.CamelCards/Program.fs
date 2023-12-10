open System
open System.IO
open System.Text.RegularExpressions

type HandType =
    | FiveOfAKind = 7
    | FourOfAKind = 6
    | FullHouse = 5
    | ThreeOfAKind = 4
    | TwoPair = 3
    | OnePair = 2
    | HighCard = 1

type PlayingCard =
    | Ace = 13
    | King = 12
    | Queen = 11
    | Ten = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2
    | Joker = 1

let ParsePlayingCard (c: char) =
    match c with
    | 'A' -> PlayingCard.Ace
    | 'K' -> PlayingCard.King
    | 'Q' -> PlayingCard.Queen
    | 'J' -> PlayingCard.Joker
    | 'T' -> PlayingCard.Ten
    | '9' -> PlayingCard.Nine
    | '8' -> PlayingCard.Eight
    | '7' -> PlayingCard.Seven
    | '6' -> PlayingCard.Six
    | '5' -> PlayingCard.Five
    | '4' -> PlayingCard.Four
    | '3' -> PlayingCard.Three
    | '2' -> PlayingCard.Two

type CamelCardsHand =
    { Id: int
      Cards: PlayingCard[]
      Bid: int64 }

let rec ExpandJokerHand (hand: CamelCardsHand) =
    match (hand.Cards |> Array.tryFindIndex (fun f -> f = PlayingCard.Joker)) with
    | Some i ->
        [| 2..13 |]
        |> Array.map (fun c ->
            hand.Cards
            |> Array.mapi (fun k v -> if k = i then (LanguagePrimitives.EnumOfValue c) else v))
        |> Array.map (fun z -> ExpandJokerHand { hand with Cards = z })
        |> Array.collect id
    // |> Array.filter (fun f -> (f |> Array.contains PlayingCard.Joker) = false)
    | None -> [| hand.Cards |]


let DetermineHandType (hand: CamelCardsHand) =
    let isFiveOfAKind hand =
        hand.Cards |> Seq.groupBy id |> Seq.length = 1
        |> fun f ->
            match f with
            | true -> Some HandType.FiveOfAKind
            | _ -> None

    let isFourOfAKind hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length) = 2 && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 4)))
        |> fun f ->
            match f with
            | true -> Some HandType.FourOfAKind
            | _ -> None

    let isFullHouse hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length) = 2 && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 3)))
        |> fun f ->
            match f with
            | true -> Some HandType.FullHouse
            | _ -> None

    let isThreeOfAKind hand =
        hand.Cards
        |> Seq.groupBy id
        |> fun f -> (f |> Seq.length = 3) && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 3))
        |> fun f ->
            match f with
            | true -> Some HandType.ThreeOfAKind
            | _ -> None

    let isTwoPair hand =
        hand.Cards
        |> Seq.groupBy id
        |> fun f ->
            (f |> Seq.length = 3)
            && (f |> Seq.filter (fun e -> (snd e) |> Seq.length = 2) |> Seq.length) = 2
        |> fun f ->
            match f with
            | true -> Some HandType.TwoPair
            | _ -> None

    let isOnePair hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length = 4))
        |> fun f ->
            match f with
            | true -> Some HandType.OnePair
            | _ -> None

    let isHighCard hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length = 5))
        |> fun f ->
            match f with
            | true -> Some HandType.HighCard
            | _ -> None

    seq {
        isFiveOfAKind
        isFourOfAKind
        isFullHouse
        isThreeOfAKind
        isTwoPair
        isOnePair
        isHighCard
    }
    |> Seq.find (fun f ->
        match f hand with
        | Some _ -> true
        | _ -> false)
    |> fun f -> (f hand).Value

let GetStrongestHand (hands: PlayingCard array array) =
    hands
    |> Array.map (fun h -> (h, LanguagePrimitives.EnumToValue(DetermineHandType { Id = 0; Bid = 0L; Cards = h })))
    |> Array.maxBy snd
    |> fst

let CompareHands (a: CamelCardsHand) (b: CamelCardsHand) =
    let strongestJokerHand =
        ExpandJokerHand
        >> GetStrongestHand
        >> (fun f -> { Id = 0; Bid = 0L; Cards = f })

    let aValue = DetermineHandType(a |> strongestJokerHand)
    let bValue = DetermineHandType(b |> strongestJokerHand)

    match aValue < bValue with
    | true -> (b, a)
    | false ->
        match aValue > bValue with
        | true -> (a, b)
        | false ->
            a.Cards
            |> Array.zip b.Cards
            |> Array.find (fun (aCard, bCard) -> aCard <> bCard)
            |> fun (bCard, aCard) ->
                match aCard < bCard with
                | true -> (b, a)
                | false -> (a, b)


let ParseHand (line: string) index =
    Regex.Match(line, "(?<hand>[^\s]+)\s(?<bid>\d+)")
    |> (fun f ->
        match f.Groups |> Seq.toArray with
        | [| _; cards; bid |] ->
            { Id = index
              Bid = bid.Value |> int64
              Cards = cards.Value |> Seq.map ParsePlayingCard |> Seq.toArray })

let ParseInput file =
    File.ReadAllLines file |> Seq.mapi (fun i f -> ParseHand f i) |> Seq.toArray

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let hands = ParseInput file

    let totalWinnings =
        hands
        |> Array.sortWith (fun a b ->
            match a.Id = b.Id with
            | true -> 0
            | false ->
                match CompareHands a b with
                | (x, _) when x.Id = a.Id -> -1
                | (x, _) when x.Id = b.Id -> 1
                | _ -> 0)
        |> Array.rev
        |> Array.zip [| 1 .. hands.Length |]
        |> Array.fold (fun acc (rank, card) -> acc + ((rank |> int64) * card.Bid)) 0L

    printfn $"[**] Total winnings: %i{totalWinnings}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
