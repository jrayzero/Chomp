module Chomp.passes

let runPasses program =
    typecheck.TypeCheck.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    let mutable program = lower.InlineConstants.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    program <- lower.LowerTransientParseBits.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    program <- lower.LowerParseLiteral.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    program <- lower.LowerParseElementAndParseTemplate.pass program
    printfn "%s" (visitor.Regenerate().visitProgram program)
    ()