module Chomp.passes

open AST

let runUnitPass (pass:program->unit) (program:program) =
    pass program
    program

let runASTPass (pass:program->program) (printAfter:bool) (program:program) =
    let p = pass(program)
    if printAfter then
        printfn "%s" (visitor.Regenerate().visitProgram p)
    p

let runPasses program =
    runUnitPass typecheck.TypeCheck.pass program
    |> runASTPass lower.InlineConstants.pass true
    |> runASTPass lower.LowerTransientParseBits.pass true
    |> runASTPass lower.LowerParseLiteral.pass true
    |> runASTPass lower.LowerParseElementAndParseTemplate.pass true
    |> runASTPass lower.LowerParseAssignments.pass true
    |> runASTPass lower.NaiveLowerAlternates.pass true