open System
open System.IO
open System.Text.RegularExpressions

type Map =
    { StepSequence: char array
      Start: Node
      End: Node
      Nodes: Map<string, Node> }

and Node =
    { Key: string
      mutable Left: Node Option
      mutable Right: Node Option }

let rec FollowSequence (start: Node) (target: Node) (directions: char array) (path: Node list) =
    match (start.Key = target.Key) with
    | true -> path
    | false ->
        let currentInstruction = directions[path.Length % directions.Length]
        match currentInstruction with
        | 'L' -> FollowSequence (start.Left.Value) target directions (path @ [start])
        | 'R' -> FollowSequence (start.Right.Value) target directions (path @ [start])
        | _ -> failwith "Unknown instruction"

let ParseInput file =
    let lines = File.ReadAllLines file
    let stepsSeq = lines[0] |> Seq.toArray
    let mappings = lines |> Seq.skip 2

    let nodeKeys =
        lines
        |> Seq.skip 2
        |> Seq.map (fun f -> f.Split(" ")[0])
        |> Seq.map (fun f -> { Key = f; Left = None; Right = None })
        |> Seq.map (fun f -> f.Key, f)
        |> Map

    nodeKeys
    |> Map.iter (fun f ->
        let key = f.Split(" ")[0];
        let s = mappings |> Seq.find (fun f -> f.StartsWith key)
        let sx = Regex.Match(s, $"\((?<from>.+), (?<to>.+)\)")
        let value = nodeKeys[key]
        value.Left <- nodeKeys.TryFind sx.Groups[1].Value
        value.Right <- nodeKeys.TryFind sx.Groups[2].Value
        ignore)

    { StepSequence = stepsSeq
      Start = nodeKeys["AAA"]
      End = nodeKeys["ZZZ"]
      Nodes = nodeKeys }

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let map = ParseInput file
    let followedPath = FollowSequence map.Start map.End map.StepSequence []
    
    printf "[*] Number of steps required from AAA -> ZZZ: %i" (followedPath |> Seq.length)
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
