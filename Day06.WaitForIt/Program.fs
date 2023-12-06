open System
open System.IO
open System.Text.RegularExpressions

type Race = { BestTime: int64; Distance: int64 }

let CalculateAccelerationTimesForRace (race: Race) =
    seq { 0L .. race.BestTime } |> Seq.map (fun f -> (f, f * (race.BestTime - f)))

let CalculateWinningAccelerationRacesForRace (race: Race) =
    CalculateAccelerationTimesForRace race
    |> Seq.filter (fun (_, distance) -> distance > race.Distance)

let CombineRaces (races: Race seq) =
    let timeTotal =
        races
        |> Seq.map (_.BestTime.ToString())
        |> Seq.reduce (fun acc ele -> acc + ele)
        |> int64

    let distanceTotal =
        races
        |> Seq.map (_.Distance.ToString())
        |> Seq.reduce (fun acc ele -> acc + ele)
        |> int64

    { Distance = distanceTotal |> int64
      BestTime = timeTotal |> int64 }

let ParseInput file =
    let lines = file |> File.ReadAllLines
    let times = Regex.Matches(lines[0], @"\d+")
    let distances = Regex.Matches(lines[1], @"\d+")

    Seq.zip times distances
    |> Seq.map (fun (time, distance) ->
        { BestTime = time.Value |> int64
          Distance = distance.Value |> int64 })

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let races = ParseInput file |> Seq.toList

    let productWinningTimes =
        races
        |> Seq.map CalculateWinningAccelerationRacesForRace
        |> Seq.map Seq.length
        |> Seq.reduce (fun acc ele -> acc * ele)

    printfn $"[*] Multiplied number of ways to beat the record: %i{productWinningTimes}"

    let combinedRace = CombineRaces races

    let combinedWinningTimes =
        CalculateWinningAccelerationRacesForRace combinedRace |> Seq.length

    printfn $"[**] Number of ways to b eat the record in longer race: %i{combinedWinningTimes}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
