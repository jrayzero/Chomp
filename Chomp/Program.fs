module CommandLine

open Chomp.parser

let testKeyword = "test"

[<EntryPoint>]
let parseCommandLine args =
    match args with
        | [| testKeyword;  "parseInline"; code |] ->
            myParser code
        | _ ->
            eprintfn "Unknown command: %s" (args |> String.concat " ")
    0