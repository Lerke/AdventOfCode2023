open System
open System.IO
open System.Text.RegularExpressions

type Almenac =
    { SeedsToPlant: int64 list
      Sections: AlmenacSection list }

and AlmenacLine =
    { DestinationRangeStart: int64
      SourceRangeStart: int64
      RangeLength: int64 }

and AlmenacSection =
    { SourceName: string
      TargetName: string
      Lines: AlmenacLine list }

type AlmenacSeedSoilMapping = Map<int64, int64>

let GetMappingForAlmenacSectionAndSeedNumber (section: AlmenacSection) (seed: int64) =
    section
    |> (fun f ->
        (f.Lines
         |> List.tryFind (fun p -> p.SourceRangeStart <= seed && seed <= (p.SourceRangeStart + p.RangeLength))))
    |> fun f ->
        match f with
        | Some z -> z |> fun f -> (f.DestinationRangeStart) + (seed - f.SourceRangeStart)
        | None -> seed

let GetMappingForSeedNumber (almenac: Almenac) (seed: int64) =
    almenac.Sections
    |> fun f ->
        f
        |> List.fold (fun acc ele -> GetMappingForAlmenacSectionAndSeedNumber ele acc) seed

let ParseAlmenacFile file =
    let seedMapRegex = Regex("seeds:.*")

    let sourceToDestinationMapRegex =
        Regex(@"\w+-to-\w+ map:\r?\n?(?:(?:\d+\s?)+\r?\n?)+")

    let input = File.ReadAllText file

    let seedsInput =
        seedMapRegex.Match(input).Value.Split(":")[1]
        |> fun f -> Regex.Matches(f, "\d+")
        |> Seq.map (fun f -> f.Value |> int64)

    let maps =
        sourceToDestinationMapRegex.Matches(input)
        |> Seq.map (fun f ->
            let lines = f.Value.Replace("\r\n", "\n").Split("\n")

            let (source, target) =
                (lines[0].Split("-")[0], lines[0].Split("-").[2].Split(" ")[0])

            let mappingLines =
                lines
                |> Seq.skip 1
                |> Seq.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
                |> Seq.map (fun f ->
                    match f.Split(" ") with
                    | [| destinationStart; sourceStart; rangeLength |] ->
                        { DestinationRangeStart = destinationStart |> int64
                          SourceRangeStart = sourceStart |> int64
                          RangeLength = rangeLength |> int64 }
                    | _ -> failwithf $"Unable to parse: {f}")

            { SourceName = source
              TargetName = target
              Lines = mappingLines |> Seq.toList })

    { SeedsToPlant = seedsInput |> Seq.toList
      Sections = maps |> Seq.toList }

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let almenac = ParseAlmenacFile file

    let locationNumbers =
        almenac.SeedsToPlant |> List.map (GetMappingForSeedNumber almenac)
    let lowestLocationNumber = locationNumbers |> List.min
    
    printf $"[*] Lowest location number: %i{lowestLocationNumber}"


    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
