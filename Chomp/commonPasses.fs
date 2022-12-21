module Chomp.commonPasses

open AST
open visitor

type GatherConstants() =
    inherit ConstVisitor()
    
    member val constants = System.Collections.Generic.Dictionary<string,literal>()
    
    static member pass x =
        let this = GatherConstants()
        this.visitProgram x
        this.constants
    
    override this.visitElement x =
        match x with
            | Constant(name,lit) -> this.constants[name] <- lit
            | _ -> base.visitElement x