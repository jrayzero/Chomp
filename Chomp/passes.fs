module Chomp.passes

let runPasses program =
    typecheck.TypeCheck.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    let program = lower.LowerTransientParseBits.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    ()