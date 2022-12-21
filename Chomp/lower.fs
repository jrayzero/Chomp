module Chomp.lower

open AST

// built-in functions (maye be converted at actual codegen)
// skipBits(expr)
// currentBitMSB(expr)
// parse(syntaxObj)
// syntaxFactory(syntaxName)
// templateFactory(syntaxName, args...)

// [x]; => skipBits(x)
type LowerTransientParseBits() =
    inherit visitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerTransientParseBits"
        LowerTransientParseBits().run x
    
    member this.run x =
        this.visitProgram x
    
    override this.visitStmt x =
        match x with
            | Rule(rule) ->
                match rule with
                    | Transient(t) ->
                        match t with
                            | ParseBits(e) ->
                                ExprStmt(Callback({name="skipBits"
                                                   args = [this.visitExpr e]}))
                            | _ -> base.visitStmt x
                    | _ -> base.visitStmt x
            | _ -> base.visitStmt x 
    

