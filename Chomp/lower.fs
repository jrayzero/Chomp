module Chomp.lower

open System.Collections.Generic
open System.Globalization
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

// wherever a constant is used, replace it with the actual value held by the constant            
type InlineConstants(constantDecls: Dictionary<string,literal>) =
    
    inherit visitor.Rebuilder()
    
    // current declarations for a scope
    // if the variable used doesn't exist in the scope (and it's in the position for a constant),
    // it's constant
    let curDecls = List<HashSet<string>>()
    
    static member pass x =
        printfn "==Running pass InlineConstants"
        let this = InlineConstants(commonPasses.GatherConstants.pass x)
        this.visitProgram x
        
    override this.visitElement x =
        match x with
            | Syntax(name,ast,body) ->
                curDecls.Add(HashSet<string>())
                let xast = ast |> List.map this.visitAnyDeclaration
                let xbody = this.visitStmt body
                curDecls.RemoveAt(curDecls.Count-1)
                Syntax(name,xast,xbody)
            | Template(name,bindings,body) ->
                curDecls.Add(HashSet<string>())
                for binding in bindings do
                    match binding with
                        | ArrayBinding(_,_,n) -> curDecls[curDecls.Count - 1].Add(n) |> ignore
                        | ScalarBinding(_,_,n) -> curDecls[curDecls.Count - 1].Add(n) |> ignore 
                let xbindings = bindings |> List.map this.visitBinding
                curDecls.RemoveAt(curDecls.Count-1)
                let xbody = this.visitStmt body
                Template(name,xbindings,xbody)
            | _ -> base.visitElement x
            
    override this.visitScalarDeclaration x =
        curDecls[curDecls.Count - 1].Add(x.name) |> ignore
        base.visitScalarDeclaration x

    override this.visitArrayDeclaration x =
        match x with
            | Stack(n,_,_) -> curDecls[curDecls.Count - 1].Add(n) |> ignore
            | Heap(n,_,_) -> curDecls[curDecls.Count - 1].Add(n) |> ignore
        base.visitArrayDeclaration x
        
    override this.visitRhs x =
        match x with
            | ParseElement(name) ->
                let mutable found = false
                for curDecl in curDecls do
                    if curDecl.Contains name then
                        found <- true
                if not found then
                    // can be a constant or a syntax
                    if constantDecls.ContainsKey name then
                        ParseLiteral(constantDecls[name])
                    else
                        base.visitRhs x
                else
                    base.visitRhs x
            | _ -> base.visitRhs x
            
    override this.visitExpr x =
        match x with
            | Variable(var) ->
                let n = var.name.levels |> String.concat "."
                if constantDecls.ContainsKey(n) then
                    Literal(constantDecls[n])
                else
                    Variable(this.visitVariable var)
            | _ -> base.visitExpr x
            
    override this.visitMarker x =
        match x with
            | ConstantMarker(name) ->
                if not (constantDecls.ContainsKey name) then
                    failwith (sprintf "Could not find Constant \"%s\" used as marker." name)
                LiteralMarker(constantDecls[name])
            | _ -> base.visitMarker x
            
// <literal>; => [nbits_in_literal]{value of literal}
type LowerParseLiteral() =
    inherit visitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerParseLiteral"
        LowerParseLiteral().run x
    
    member this.run x =
        this.visitProgram x
    
    override this.visitRhs x =
        match x with
            | ParseLiteral(lit) ->
                // determine the number of bits needed for the literal
                let nbits =
                    match lit.lit with
                        | Hex -> lit.value.Length * 4
                        | Decimal -> 64
                        | Binary -> lit.value.Length
                        | Ascii -> lit.value.Length * 8
                // now convert to a decimal value
                let dec =
                    match lit.lit with
                        | Hex ->
                            if nbits > 64 then
                                failwith (sprintf "Parsed hex value 0x%s does not fit in 64-bit integer" lit.value)
                            System.Int64.Parse(lit.value, NumberStyles.HexNumber)
                        | Decimal ->
                            System.Int64.Parse(lit.value)
                        | Binary ->
                            if nbits > 64 then
                                failwith (sprintf "Parsed binary value 0b%s does not fit in 64-bit integer" lit.value)
                            let mutable binary: int64 = 0
                            let mutable idx = 0
                            for c in lit.value do
                                if c = '1' then
                                    let v = int64(1) <<< idx
                                    binary <- binary + v
                                idx <- idx + 1
                            binary
                        | Ascii ->
                            if nbits > 64 then
                                failwith (sprintf "Parsed ascii value \"%s\" does not fit in 64-bit integer" lit.value)                            
                            let mutable ascii: int64 = 0
                            for c in lit.value do
                                let cx = int64(c)
                                ascii <- (ascii ||| cx)
                                ascii <- ascii <<< 8
                            ascii
                let newLit = Literal({lit=AST.Decimal; value=sprintf "%d" nbits})                            
                ParseBitsAndValidate(newLit, [Single(dec)])
            | _ -> base.visitRhs x

