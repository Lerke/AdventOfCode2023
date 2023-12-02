open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

type DiceColor =
    | Blue
    | Red
    | Green

type GameRules = { DicePerBag: Map<DiceColor, int> }

let DiceColor (color: string) =
    match color.ToLower() with
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | _ -> failwith "Invalid color"

type DiceRound = (int * DiceColor) list

type DiceGame = { Id: int; Round: DiceRound list }

let ParseRoundInfo (line: string) =
    match line.Split(";") with
    | x when x.Length > 0 ->
        x
        |> Seq.map (fun f -> Regex.Matches(f, @"(?<number>\d+)\s(?<color>\w+)"))
        |> Seq.map (fun f -> f |> Seq.map (fun m -> (m.Groups[1].Value |> int, DiceColor m.Groups[2].Value)))
        |> Seq.map (fun f -> (f |> Seq.toList))
        |> Seq.toList
    | _ -> failwith $"Could not parse input line: {line}"

let ParseInputLine (line: string) =
    match line.Split(":") with
    | [| game; roundInfo |] ->
        { Id = Regex.Match(game, "\d+").Value |> int
          Round = ParseRoundInfo roundInfo }
    | _ -> failwithf $"Could not parse input line: {line}"

let ParsePuzzleInput file =
    File.ReadAllLines file |> Seq.map ParseInputLine

let DiceUsedInGame (game: DiceGame) =
    game.Round
    |> Seq.collect id
    |> Seq.groupBy (fun (_, color) -> color)
    |> Seq.map (fun (color, round) -> (color, round |> Seq.map fst |> Seq.sum))
    |> Map.ofSeq

let DiceUsedInRound (round: DiceRound) =
    round
    |> Seq.groupBy (fun (_, color) -> color)
    |> Seq.map (fun (color, round) -> (color, round |> Seq.map fst |> Seq.sum))
    |> Map.ofSeq

let RoundPossibleWithRules (rules: GameRules) (round: DiceRound) =
    let diceUsed = DiceUsedInRound round

    rules.DicePerBag
    |> Seq.forall (fun rule ->
        match diceUsed.ContainsKey rule.Key with
        | true -> rule.Value >= diceUsed[rule.Key]
        | false -> true)

let GamePossibleWithRules (rules: GameRules) (game: DiceGame) =
    game.Round |> Seq.forall (RoundPossibleWithRules rules)

let MinimumCubesNecessaryPerGame (game: DiceGame) =
    let min =
        game.Round
        |> Seq.collect (fun g -> g)
        |> Seq.groupBy (fun (_, g) -> g)
        |> Seq.map (fun (color, rounds) -> (color, fst (rounds |> Seq.maxBy (fun f -> fst f))))

    Map
        [ (Blue, snd (min |> Seq.maxBy (fun f -> (fst f) = Blue)))
          (Red, snd (min |> Seq.maxBy (fun f -> (fst f) = Red)))
          (Green, snd (min |> Seq.maxBy (fun f -> (fst f) = Green))) ]

let Power (maxResults: Map<DiceColor, int>) =
    maxResults |> Map.values |> (Seq.reduce (fun p c -> p * c))

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let partOneRules = { DicePerBag = Map [ (Red, 12); (Green, 13); (Blue, 14) ] }
    let ruleChecker = GamePossibleWithRules partOneRules
    let input = ParsePuzzleInput file

    let possibleGames =
        input
        |> Seq.map ruleChecker
        |> Seq.zip input
        |> Seq.filter (fun (_, possible) -> possible = true)

    let sumOfPossibleGames = possibleGames |> Seq.sumBy (fun (game, _) -> game.Id)

    printfn $"[*] Sum of possible games: %i{sumOfPossibleGames}"

    let minCubes = input |> Seq.map MinimumCubesNecessaryPerGame
    let powers = minCubes |> Seq.map Power
    let powerSum = powers |> Seq.sum
    printfn $"[**] Sum of power of minimum cube sets %i{powerSum}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
