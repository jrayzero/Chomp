module Chomp.passes

open AST
open Backend

let runUnitPass pass program =
    pass program
    program

let runASTPass pass printAfter program =
    let p = pass(program)
    if printAfter then
        printfn "%s" (ASTVisitor.Regenerate().visitProgram p)
    p

let runPasses program =
    runUnitPass typecheck.TypeCheck.pass program
    |> runASTPass lower.InlineConstants.pass true
    |> runASTPass lower.LowerTransientParseBits.pass true
    |> runASTPass lower.LowerParseLiteral.pass true
    |> runASTPass lower.LowerParseElementAndParseTemplate.pass true
    |> runASTPass lower.LowerParseAssignments.pass true
    |> runASTPass lower.NaiveLowerAlternates.pass true
    |> runASTPass lower.LowerLiterals.pass true
    |> runASTPass lower.LowerUserCallbacks.pass true
    |> runASTPass lower.LowerPushPop.pass true
    |> fun y -> (
            let prog2 = imperativeIR.ASTToImperativeIR.pass y
            printfn "%A" prog2
            prog2
        )
    |> codegen.LowerFunctions.pass 
    |> codegen.CodegenCPP.pass
    |> (fun c -> printfn "%s" c; c)