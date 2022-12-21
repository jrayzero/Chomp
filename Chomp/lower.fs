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
        LowerTransientParseBits().visitProgram x
    
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
        let this = InlineConstants(common.GatherConstants.pass x)
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
        LowerParseLiteral().visitProgram x
    
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

// constants must already be lowered
// <syntax> => parse_syntax(syntax_factory(syntaxName))
// <template(...)> => parse_template(template_factory(templateName, ...))
type LowerParseElementAndParseTemplate() =
    inherit visitor.Rebuilder()
        
    static member pass x =
        printfn "==Running pass LowerParseElementAndParseTemplate"
        LowerParseElementAndParseTemplate().visitProgram x
        
    override this.visitRhs x =
        match x with
            | ParseElement(name) ->
                Expr(builtins.syntaxParserFactory name)
            | ParseTemplate(name,bindings) ->
                let xbindings = bindings |> List.map this.visitExpr
                Expr(builtins.templateParserFactory name xbindings)
            | _ -> base.visitRhs x

// transient parsebits should be gone already
// lhs := [5]; =>
// if !exists(buffer, cursor, 5) { fatal("Not enough bits"); }
// lhs := parseBits(buffer, cursor, 5);
//
// lhs := [5]{ranges...} =>
// if !exists(buffer, cursor, 5) { fatal("Not enough bits"); }
// lhsTmp := parseBits(buffer, cursor, 5);
// if !(lhs within ranges...) { fatal("Invalid value"); }
// lhs := lhsTmp
type LowerParseAssignments() =
    inherit visitor.Rebuilder()
    
    member private this.visitRhsInner r =
        let failNbits = builtins.fatal "Not enough bits left"
        match r with
            | ParseBits(ex) ->
                let nbits = this.visitExpr ex
                let cond = Not(builtins.exists nbits)
                let enough = IfElse(cond, Suite([ExprStmt(failNbits)]), None)
                let parseBits = Callback({name="parseBits"
                                          args=[Variable(builtins.bufferVar)
                                                Variable(builtins.cursorVar)
                                                nbits]})
                Some((enough,Expr(parseBits)))
            | ParseBitsAndValidate(ex, ranges) ->
                let nbits = this.visitExpr ex
                let cond = Not(builtins.exists nbits)
                let enough = IfElse(cond, Suite([ExprStmt(failNbits)]), None)
                let parseBits = Callback({name="parseBits"
                                          args=[Variable(builtins.bufferVar)
                                                Variable(builtins.cursorVar)
                                                nbits]})
                // assign to a temporaryId
                let tmpVar = common.uniqueVar (Some("tmpParse"))
                let parsed = Rule(ScalarDeclarationAssign({name=tmpVar.name.levels[0];t=Int64(true)},
                                                          Some(Expr(parseBits))))
                // check all the ranges
                let conds = List<expr>()
                for range in ranges do
                    let cond = 
                        match range with
                            | Single(i) -> Equals(true, [Variable(tmpVar)
                                                         Literal({lit=Decimal;value=sprintf "%d" i})])
                            | Lower(i) -> GreaterThan(true, Variable(tmpVar),
                                                      Literal({lit=Decimal;value=sprintf "%d" i}))
                            | Upper(i) -> LessThan(true, Variable(tmpVar),
                                                   Literal({lit=Decimal;value=sprintf "%d" i}))
                            | Range(l,u) -> And([GreaterThan(true, Variable(tmpVar),
                                                        Literal({lit=Decimal;value=sprintf "%d" l}));
                            LessThan(true, Variable(tmpVar), Literal({lit=Decimal;value=sprintf "%d" u}))])
                    conds.Add(cond)
                let items = List.ofSeq(conds |> Seq.map id)
                let cond = Not(Or(items))
                let checkRanges = IfElse(cond,
                                         Suite([ExprStmt(builtins.fatal "Parsed value doesn't match range")]),
                                         None)
                Some((Suite([enough;parsed;checkRanges]),Expr(Variable(tmpVar))))
            | _ -> None
    
    static member pass x =
        printfn "==Running pass LowerParseAssignments"
        LowerParseAssignments().visitProgram x
        
    override this.visitStmt x =
        match x with
            | Rule(rule) ->
                match rule with
                    | ScalarDeclarationAssign(l,r) ->
                        match r with
                            | Some(r) ->
                                let res = this.visitRhsInner r
                                match res with
                                    | Some(a,b) ->
                                        Suite([a
                                               Rule(ScalarDeclarationAssign(this.visitScalarDeclaration l, Some(b)))])
                                    | None ->
                                        Rule(ScalarDeclarationAssign(this.visitScalarDeclaration l, Some(this.visitRhs r)))
                            | None -> base.visitStmt x
                    | Assignment(l,r) ->
                        let res = this.visitRhsInner r
                        match res with
                            | Some(a,b) ->
                                Suite([a
                                       Rule(Assignment(this.visitLhs l, b))])
                            | None ->
                                Rule(Assignment(this.visitLhs l, this.visitRhs r))  
                    | _ -> base.visitStmt x
            | _ -> base.visitStmt x    
    