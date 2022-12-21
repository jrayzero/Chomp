module Chomp.passes

let runPasses program =
    typecheck.TypeCheck().pass program
    // typecheck.Typecheck.pass program
    ()