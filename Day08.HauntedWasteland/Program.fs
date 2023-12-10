open System

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    0 |> ignore
| _ ->
    printf "Usage: dotnet run /path/to/file"
    1 |> ignore
