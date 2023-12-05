open System
open System.IO
open System.IO.MemoryMappedFiles
open System.Text.RegularExpressions

type Almenac =
    { SeedsToPlant: int list
      Sections: AlmenacSection list }

and AlmenacLine =
    { DestinationRangeStart: int
      SourceRangeStart: int
      RangeLength: int }

and AlmenacSection =
    { SourceName: string
      TargetName: string
      Lines: AlmenacLine list }

let ParseAlmenacFile file =
    let seedMapRegex = Regex("seeds:.*")

    let sourceToDestinationMapRegex =
        Regex(@"\w+-to-\w+ map:.*?(?:^$|\Z)", RegexOptions.Singleline ||| RegexOptions.Multiline)

    let input = File.ReadAllText file

    let seedsInput =
        seedMapRegex.Match(input).Value.Split(":")[1]
        |> fun f -> Regex.Matches(f, "\d+")
        |> Seq.map (fun f -> f.Value |> int)

    let maps =
        sourceToDestinationMapRegex.Matches(input)
        |> Seq.map (fun f ->
            let lines = f.Value.Split Environment.NewLine

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
                          RangeLength = rangeLength |> int })

            { SourceName = source
              TargetName = target
              Lines = mappingLines |> Seq.toList })

    maps

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let almenac = ParseAlmenacFile file
    printfn "%A" almenac
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
