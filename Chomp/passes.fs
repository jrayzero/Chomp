module Chomp.passes

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
    |> fun y -> (
            let prog2 = imperativeIR.ASTToImperativeIR.pass y
            printfn "%A" prog2
            prog2
        )   