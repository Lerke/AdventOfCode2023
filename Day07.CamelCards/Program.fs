open System
open System.IO
open System.Text.RegularExpressions

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let HandTypeValue handType =
    match handType with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1

type PlayingCard =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | One

let PlayingCard (c: char) =
    match c with
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | '1' -> One

type CamelCardsHand = { Id: int; Cards: PlayingCard[]; Bid: int }

let DetermineHandType (hand: CamelCardsHand) =
    let isFiveOfAKind hand =
        hand.Cards |> Seq.groupBy id |> Seq.length = 1
        |> fun f ->
            match f with
            | true -> Some FiveOfAKind
            | _ -> None

    let isFourOfAKind hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length) = 2 && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 4)))
        |> fun f ->
            match f with
            | true -> Some FourOfAKind
            | _ -> None

    let isFullHouse hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length) = 2 && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 3)))
        |> fun f ->
            match f with
            | true -> Some FullHouse
            | _ -> None

    let isThreeOfAKind hand =
        hand.Cards
        |> Seq.groupBy id
        |> fun f -> (f |> Seq.length = 3) && (f |> Seq.exists (fun e -> (snd e) |> Seq.length = 3))
        |> fun f ->
            match f with
            | true -> Some ThreeOfAKind
            | _ -> None

    let isTwoPair hand =
        hand.Cards
        |> Seq.groupBy id
        |> fun f ->
            (f |> Seq.length = 3)
            && (f |> Seq.filter (fun e -> (snd e) |> Seq.length = 2) |> Seq.length) = 2
        |> fun f ->
            match f with
            | true -> Some TwoPair
            | _ -> None

    let isOnePair hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length = 4))
        |> fun f ->
            match f with
            | true -> Some TwoPair
            | _ -> None

    let isHighCard hand =
        hand.Cards
        |> Seq.groupBy id
        |> (fun f -> (f |> Seq.length = 5))
        |> fun f ->
            match f with
            | true -> Some TwoPair
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


let ParseHand (line: string) index =
    Regex.Match(line, "(?<hand>[^\s]+)\s(?<bid>\d+)")
    |> (fun f ->
        match f.Groups |> Seq.toArray with
        | [| _; cards; bid |] ->
            { Id = index
              Bid = bid.Value |> int
              Cards = cards.Value |> Seq.map PlayingCard |> Seq.toArray }
        | groups -> failwith $"Could not parse: %s{f.Value}")

let ParseInput file = File.ReadAllLines file |> Seq.mapi (fun i f -> ParseHand f i) |> Seq.toArray

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let hands = ParseInput file
    let types = hands |> Seq.map DetermineHandType |> Seq.toArray
    printfn "%A" hands
    printfn "%A" types
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
