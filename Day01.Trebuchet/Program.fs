open System
open System.IO
open System.Text.RegularExpressions
open System.Linq

let Replacements =
    Map.ofList
        [ ("one", "1")
          ("two", "2")
          ("three", "3")
          ("four", "4")
          ("five", "5")
          ("six", "6")
          ("seven", "7")
          ("eight", "8")
          ("nine", "9") ]

let FindReplacementIndices (line: string) =
    Replacements.Keys
    |> Seq.map (fun f ->
        (f,
         Regex.Matches(line, f)
         |> Seq.collect (fun g -> g.Groups)
         |> Seq.filter (fun g -> g.Success)
         |> Seq.map (fun g -> (g.Index, g.Index + g.Length))))
    |> Seq.filter (fun (f, g) -> g.Count() > 0)
    |> Seq.map (fun (f, g) -> (g |> Seq.sort |> Seq.map (fun (p, q) -> ((p), f))))
    |> Seq.collect id
    |> Seq.sort
    |> Seq.toList

let rec ReplacePuzzleLine (line: string) (idx: int) (result: string) =
    let replacementIndices = FindReplacementIndices line

    match idx < line.Length with
    | true ->
        match (replacementIndices) |> (Seq.tryFindIndex (fun (p, q) -> p = idx)) with
        | None -> ReplacePuzzleLine (line) (idx + 1) (result + (line[idx] |> string))
        | Some i ->
            let item = replacementIndices.[i]
            let replacement = Replacements[snd item]
            ReplacePuzzleLine (line) (idx + (snd item).Length - 1) (result + replacement)
    | _ -> result

let ParsePuzzleLine (line: string) =
    match Regex.Match(line, @".*?(\d)(?:.*(\d).*)?") with
    | m when m.Success ->
        match (m.Groups.Values.ToList() |> List.ofSeq) with
        | [ (_: Group); (g2: Group); (g3: Group) ] when g2.Success && g3.Success -> (g2.Value + g3.Value) |> int
        | [ (_: Group); (g2: Group); (_: Group) ] when g2.Success -> (g2.Value + g2.Value) |> int
        | _ -> failwith $"Could not parse input line: {line}"
    | _ -> failwith $"Could not parse input line: {line}"

let ParsePuzzleInput file =
    File.ReadAllLines file
    |> Seq.map (fun f -> ReplacePuzzleLine f 0 "")
    |> Seq.map ParsePuzzleLine
    |> Seq.sum

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    (let sum = ParsePuzzleInput file
     printf $"[**] Sum: {sum}"
     0)
    |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
