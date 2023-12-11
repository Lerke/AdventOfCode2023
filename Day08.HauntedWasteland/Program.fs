open System
open System.IO
open System.Text.RegularExpressions

type Map =
    { StepSequence: char array
      Nodes: Map<string, Node> }

and Node =
    { Key: string
      mutable Left: Node Option
      mutable Right: Node Option }

let PerformStep (start: Node) (directions: char array) (stepsTaken: int64) =
    let currentInstruction = directions[(stepsTaken % (directions.Length |> int64)) |> int]
    match currentInstruction with
    | 'L' -> start.Left.Value
    | 'R' -> start.Right.Value

let rec FollowSequence (start: Node) (target: Node) (directions: char array) (path: Node list) =
    match (start.Key = target.Key) with
    | true -> path
    | false ->
        let currentInstruction = directions[path.Length % directions.Length]
        match currentInstruction with
        | 'L' -> FollowSequence (start.Left.Value) target directions (path @ [start])
        | 'R' -> FollowSequence (start.Right.Value) target directions (path @ [start])

let rec GhostPathing (starts: Node list) (directions: char array) (stepsTaken: int64) =
    printfn $"Steps taken: %i{stepsTaken}"
    let allFinished = starts |> List.forall (fun f -> f.Key.EndsWith "Z")
    match allFinished with
    | true -> stepsTaken
    | false ->
        let newStarts = starts |> List.map (fun f -> PerformStep f directions stepsTaken)
        GhostPathing newStarts directions (stepsTaken + 1L)

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
      Nodes = nodeKeys }

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let map = ParseInput file
    // let followedPath = FollowSequence map.Nodes["AAA"] map.Nodes["ZZZ"] map.StepSequence []
    // printfn "[*] Number of steps required from AAA -> ZZZ: %i" (followedPath |> Seq.length)
    
    let ghostStartingNodes =
        map.Nodes
        |> Map.filter (fun f _ -> f.EndsWith("A"))
        
    let followedGhostPath = GhostPathing (ghostStartingNodes.Values |> List.ofSeq) map.StepSequence 0
    printfn "[**] Number of steps taken before only on nodes ending with Z: %i" followedGhostPath
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
