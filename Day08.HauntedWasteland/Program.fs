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

let rec FollowSequence (start: Node) (target: Node) (directions: char array) (path: Node list) =
    match (start.Key = target.Key) with
    | true -> path
    | false ->
        let currentInstruction = directions[path.Length % directions.Length]
        match currentInstruction with
        | 'L' -> FollowSequence (start.Left.Value) target directions (path @ [start])
        | 'R' -> FollowSequence (start.Right.Value) target directions (path @ [start])

let rec DetermineFullPath (starts: Node) (directions: char array) (path: (Node * string) List) =
    let pathIdx = path.Length % directions.Length
    let remainingDirections = directions |> Array.skip pathIdx
    let seenBefore = path |> List.tryFindIndex (fun f -> (fst f).Key = starts.Key && (snd f) = (String(remainingDirections)))
    match seenBefore with
    | Some x -> path @ [ (starts, String(remainingDirections)) ]
    | None -> 
        let appendedPath = path @ [ (starts, String(remainingDirections)) ]
        let currentInstruction = remainingDirections[0]
        match currentInstruction with
        | 'L' -> DetermineFullPath (starts.Left.Value) directions appendedPath
        | 'R' -> DetermineFullPath (starts.Right.Value) directions appendedPath

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

let rec gcd (x: int64) (y: int64) = if y = 0 then abs x else gcd y (x % y)

let lcm (a: int64) (b: int64) = a * b / (gcd a b)

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    let map = ParseInput file
    let followedPath = FollowSequence map.Nodes["AAA"] map.Nodes["ZZZ"] map.StepSequence []
    printfn $"[*] Number of steps required from AAA -> ZZZ: %i{followedPath |> Seq.length}"
    
    let ghostStartingNodes =
        map.Nodes
        |> Map.filter (fun f _ -> f.EndsWith("A"))
        
    let followedPath =
        ghostStartingNodes
        |> Map.values
        |> Seq.map (fun f -> DetermineFullPath f map.StepSequence [] )
        |> Seq.toList
        
    let shortestPath =
        followedPath
        |> Seq.map(fun f -> (f |> List.findIndex (fun z -> (fst z).Key.EndsWith "Z") |> int64))
        |> Seq.pairwise
        |> Seq.map (fun f -> lcm (fst f) (snd f))
        |> Seq.reduce (fun acc ele -> lcm acc ele)
        
    printfn $"[**] Number of steps taken before only on nodes ending with Z: %i{shortestPath}"
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
