module Chomp.Program

open AST.parser
open AST.ASTVisitor
open passes

let enterParse (code: string list) =
    if code.Length = 0 then
        eprintfn "Expected code to parse!"
        1
    else
        parseIt (code |> String.concat "\n") |> fun (x,_) -> runPasses x |> ignore
        0
        
let codegenCPP (code: string list) ofn =
    if code.Length = 0 then
        eprintfn "Expected code to parse!"
        1
    else
        let code = parseIt (code |> String.concat "\n") |> fun (x,_) -> runPasses x
        System.IO.File.WriteAllText(ofn, code)
        0    
        
let checkAST (code: string list) =
    if code.Length = 0 then
        eprintfn "Expected code to parse!"
        1
    else
        let chompAST,fparsecAST = parseIt (code |> String.concat "\n")
        // regenerate the code and write to temp file
        let regenCode = Regenerate().visitProgram chompAST
        let tmp = System.IO.Path.GetTempFileName()
        printfn "%s" tmp
        System.IO.File.WriteAllText(tmp, regenCode)
        // reparse it
        let code2 = List.ofSeq (System.IO.File.ReadLines(tmp))
        let chompASTR,fparsecASTR = parseIt (code2 |> String.concat "\n")
        if chompAST = chompASTR then
            printfn "ASTs match!"
        else
            eprintfn "ASTs do not match in file %s!" tmp
        0
        
[<EntryPoint>]
let parseCommandLine args =
    printfn "%A" args
    match List.ofArray args with
        | "test"::"checkAST"::[fname] ->
            checkAST(List.ofSeq (System.IO.File.ReadLines(fname)))
        | "test"::"parseFile"::[fname] ->
            enterParse (List.ofSeq (System.IO.File.ReadLines(fname)))
        | "test"::"parseInline"::code ->
            enterParse code
        | "codegen"::ifname::[ofname] ->
            codegenCPP (List.ofSeq (System.IO.File.ReadLines(ifname))) ofname
        | _ ->
            eprintfn "Unknown command: %s" (args |> String.concat " ")
            0