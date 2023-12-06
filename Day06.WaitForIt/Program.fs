open System
open System.IO
open System.Text.RegularExpressions

type Race = { BestTime: int; Distance: int }

let CalculateAccelerationTimesForRace (race: Race) =
    seq { 0 .. race.BestTime } |> Seq.map (fun f -> (f, f * (race.BestTime - f)))

let CalculateWinningAccelerationRacesForRace (race: Race) =
    CalculateAccelerationTimesForRace race
    |> Seq.filter (fun (_, distance) -> distance > race.Distance)

let ParseInput file =
    let lines = file |> File.ReadAllLines
    let times = Regex.Matches(lines[0], @"\d+")
    let distances = Regex.Matches(lines[1], @"\d+")

    Seq.zip times distances
    |> Seq.map (fun (time, distance) ->
        { BestTime = time.Value |> int
          Distance = distance.Value |> int })

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let races = ParseInput file |> Seq.toList

    let productWinningTimes =
        races
        |> Seq.map CalculateWinningAccelerationRacesForRace
        |> Seq.map Seq.length
        |> Seq.reduce (fun acc ele -> acc * ele)

    printfn $"[*] Multiplied number of ways to beat the record: %i{productWinningTimes}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
