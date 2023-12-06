open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Text.RegularExpressions

type Almenac = { SeedsToPlant: int list; Sections: AlmenacSection list }

and AlmenacLine = { DestinationRangeStart: int; SourceRangeStart: int; RangeLength: int }

and AlmenacSection = { SourceName: string; TargetName: string; Lines: AlmenacLine list }

type AlmenacSeedSoilMapping = Map<int, int>

let GetMappingForSeedNumber (almenac: Almenac) (target: string) (seed: int) =
    almenac.Sections
    |> Seq.find (fun f -> f.TargetName = target)
    |> (fun f ->
            (f.Lines
             |> List.tryFind (fun p -> p.SourceRangeStart <= seed && seed <= (p.SourceRangeStart + p.RangeLength))))
    |> fun f ->
        match f with
        | Some z ->
            z |> fun f -> (f.DestinationRangeStart) + (seed - f.SourceRangeStart)
        | None -> seed

let ParseAlmenacFile file =
    let seedMapRegex = Regex("seeds:.*")

    let sourceToDestinationMapRegex =
        Regex(@"\w+-to-\w+ map:\r?\n?(?:(?:\d+\s?)+\r?\n?)+")

    let input = File.ReadAllText file

    let seedsInput =
        seedMapRegex.Match(input).Value.Split(":")[1]
        |> fun f -> Regex.Matches(f, "\d+")
        |> Seq.map (fun f -> f.Value |> int)

    let maps =
        sourceToDestinationMapRegex.Matches(input)
        |> Seq.map (fun f ->
            let lines = f.Value.Split ("\r\n")

            let (source, target) =
                (lines[0].Split("-")[0], lines[0].Split("-").[2].Split(" ")[0])

            let mappingLines =
                lines
                |> Seq.skip 1
                |> Seq.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
                |> Seq.map (fun f ->
                    match f.Split(" ") with
                    | [| destinationStart; sourceStart; rangeLength |] ->
                        { DestinationRangeStart = destinationStart |> int
                          SourceRangeStart = sourceStart |> int
                          RangeLength = rangeLength |> int }
                    | _ -> failwithf $"Unable to parse: {f}")

            { SourceName = source; TargetName = target; Lines = mappingLines |> Seq.toList })

    { SeedsToPlant = seedsInput |> Seq.toList; Sections = maps |> Seq.toList }

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let almenac = ParseAlmenacFile file

    let test =
        { SeedsToPlant = []
          Sections =
            [ { SourceName = "seed"
                TargetName = "soil"
                Lines =
                  [ { DestinationRangeStart = 50; SourceRangeStart = 98; RangeLength = 2 }
                    { DestinationRangeStart = 52; SourceRangeStart = 50; RangeLength = 48 } ] } ] }

    let locationMapping = GetMappingForSeedNumber almenac "location"
    
    let bx = locationMapping 79
    assert ((locationMapping 79) = 82)
    assert ((locationMapping 14) = 43)
    assert ((locationMapping 55) = 86)
    assert ((locationMapping 13) = 35)
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
