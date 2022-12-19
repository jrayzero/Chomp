module CommandLine

open Chomp.parser

let testKeyword = "test"

let enterParse (code: string list) =
    if code.Length = 0 then
        eprintfn "Expected code to parse!"
        1
    else
        parseIt (code |> String.concat "")
        0    

[<EntryPoint>]
let parseCommandLine args =
    printfn "%A" args
    match List.ofArray args with
        | testKeyword::"parseFile"::[fname] ->
            enterParse (List.ofSeq (System.IO.File.ReadLines(fname)))
        | testKeyword::"parseInline"::code ->
            enterParse code
        | _ ->
            eprintfn "Unknown command: %s" (args |> String.concat " ")
            0