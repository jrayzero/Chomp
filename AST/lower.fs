module AST.lower

open System.Collections.Generic
open System.Globalization
open AST
open AST.AST

// [x]; => skipBits(x)
type LowerTransientParseBits() =
    inherit ASTVisitor.Rebuilder()
    
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
                                ExprStmt(builtins.skipBits (this.visitExpr e))
                            | _ -> base.visitStmt x
                    | _ -> base.visitStmt x
            | _ -> base.visitStmt x

// wherever a constant is used, replace it with the actual value held by the constant            
type InlineConstants(constantDecls: Dictionary<string,literal>) =
    
    inherit ASTVisitor.Rebuilder()
    
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
    inherit ASTVisitor.Rebuilder()
    
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
                            common.binaryStringToDec lit.value
                        | Ascii ->
                            if nbits > 64 then
                                failwith (sprintf "Parsed ascii value \"%s\" does not fit in 64-bit integer" lit.value)
                            common.asciiStringToDec lit.value
                let newLit = Literal({lit=AST.Decimal; value=sprintf "%d" nbits})                            
                ParseBitsAndValidate(newLit, [Single(dec)])
            | _ -> base.visitRhs x

// constants must already be lowered
// <syntax> => parse_syntax(syntaxName)
// <template(...)> => parse_template(templateName, ...)
type LowerParseElementAndParseTemplate() =
    inherit ASTVisitor.Rebuilder()
        
    static member pass x =
        printfn "==Running pass LowerParseElementAndParseTemplate"
        LowerParseElementAndParseTemplate().visitProgram x
        
    override this.visitRhs x =
        match x with
            | ParseElement(name) ->
                Expr(builtins.syntaxParse name)
            | ParseTemplate(name,bindings) ->
                let xbindings = bindings |> List.map this.visitExpr
                Expr(builtins.templateParse name xbindings)
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
//
// also lower ParseBitsAndValidates that don't have a lhs
// [5]{ranges...} =>
// if !exists(buffer, cursor, 5) { fatal("Not enough bits"); }
// tmp := parseBits(buffer, cursor, 5);
// if !(tmp within ranges...) { fatal("Invalid value"); }
type LowerParseAssignments() =
    inherit ASTVisitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerParseAssignments"
        LowerParseAssignments().visitProgram x    
    
    member private this.visitRhsInner r =
        let failNbits = builtins.fatal "Not enough bits left"
        match r with
            | ParseBits(ex) ->
                let nbits = this.visitExpr ex
                let cond = Not(builtins.exists nbits)
                let enough = IfElse(cond, Suite([ExprStmt(failNbits)]), None)
                let parseBits = builtins.parseBits nbits "int64"
                // let parseBits = Callback({name="parseBits"
                                          // args=[Variable(builtins.bufferVar)
                                                // Variable(builtins.cursorVar)
                                                // nbits]})
                Some((enough,Expr(parseBits)))
            | ParseBitsAndValidate(ex, ranges) ->
                let nbits = this.visitExpr ex
                let cond = Not(builtins.exists nbits)
                let enough = IfElse(cond, Suite([ExprStmt(failNbits)]), None)
                let parseBits = builtins.parseBits nbits "int64"
                // let parseBits = Callback({name="parseBits"
                                          // args=[Variable(builtins.bufferVar)
                                                // Variable(builtins.cursorVar)
                                                // nbits]})
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
                    | Transient(r) ->
                        let res = this.visitRhsInner r
                        match res with
                            | Some(a,_) ->
                                Suite([a])
                            | None ->
                                Rule(Transient(this.visitRhs r))
                    | _ -> base.visitStmt x
            | _ -> base.visitStmt x
            
// just lower to decimals
type LowerLiterals() =
    inherit ASTVisitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerLiterals"
        LowerLiterals().visitProgram x
    
    override this.visitLiteral x =
        // let nbits =
        //     match x.lit with
        //         | Hex -> x.value.Length * 4
        //         | Decimal -> 64
        //         | Binary -> x.value.Length
        //         | Ascii -> x.value.Length * 8
        // now convert to a decimal value
        match x.lit with
            | Hex ->
                if x.value.Length * 3 > 64 then
                    failwith (sprintf "Parsed hex value 0x%s does not fit in 64-bit integer" x.value)
                {lit=AST.Decimal; value=sprintf "%d" (System.Int64.Parse(x.value, NumberStyles.HexNumber))}
            | Decimal ->
                x
            | Binary ->
                if x.value.Length > 64 then
                    failwith (sprintf "Parsed binary value 0b%s does not fit in 64-bit integer" x.value)
                {lit=AST.Decimal; value=sprintf "%d" (common.binaryStringToDec x.value)}
            | Ascii ->
                x
    

// just to get something going
// should be all literal markers at this point
// alternate {
//   marker <lit> { A; }
//   marker <lit2> { B; }
// }
// =>
// if exists(cursor, stop, nbits) &&  lookaheadBits(nbits) = lit {
//    A;
//    skipBits(nbits)
// } else {
//    if exists(cursor, stop, nbits) &&  lookaheadBits(nbits2) = lit2 { B; skipBits(nbit2); }
//    else { fatal "Alternate failed"; }  
// }
type NaiveLowerAlternates() =
    inherit ASTVisitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass NaiveLowerAlternates"
        NaiveLowerAlternates().visitProgram x
        
    override this.visitStmt x =
        match x with
            | Alternate(options) ->
                let tmp = common.uniqueVar (Some("alt"))
                let decl = ScalarDeclarationAssign({name=tmp.name.levels[0];t=Int64(true)}, None)
                // build all the lookaheads, literals to match on, bits per literal, and bodies for each option
                let items = options |> List.map (fun (m,body) ->
                    let value,nbits = 
                        match m with
                            | LiteralMarker(lit) ->
                                match lit.lit with
                                    | Hex -> System.Int64.Parse(lit.value, NumberStyles.HexNumber),lit.value.Length * 4
                                    | Decimal -> System.Int64.Parse(lit.value), 64
                                    | Binary -> common.binaryStringToDec lit.value, lit.value.Length
                                    | Ascii -> common.asciiStringToDec lit.value, lit.value.Length * 8
                            | _ -> failwith "Constant markers should be lowered to literal!"
                    let lookahead = builtins.lookaheadBits (Literal({lit=Decimal; value=sprintf "%d" nbits})) "int64"
                    (lookahead, value, Literal({lit=Decimal;value=sprintf "%d" nbits}), body)
                    )
                let mutable ifElse = Suite([])
                for i in items.Length-1..-1..0 do
                    let lk,lit,nbits,body = items[i]
                    let cond = And([builtins.exists(nbits); Equals(true, [lk;Literal({lit=Decimal;value=sprintf "%d" lit})])])
                    if i = items.Length - 1 then
                        // innermost
                        ifElse <- IfElse(cond,
                                        Suite([ExprStmt(builtins.skipBits nbits);body]),
                                        Some(ExprStmt(builtins.fatal "Alternate failed")))
                    else
                        ifElse <- IfElse(cond,
                                        Suite([ExprStmt(builtins.skipBits nbits);body]), Some(ifElse))                        
                    
                ifElse
            | _ -> base.visitStmt x

// TODO check that push and pop exist within the same scope!            
type LowerPushPop() =
    inherit ASTVisitor.Rebuilder()
    
    let buffers = System.Collections.Generic.Stack<variable>()
    let cursors = System.Collections.Generic.Stack<variable>()
    let stops = System.Collections.Generic.Stack<variable>()
    
    static member pass x =
        printfn "==Running pass LowerPushPop"
        LowerPushPop().visitProgram x
        
    override this.visitStmt x =
        match x with
            | AST.Push(buff,cur,stop) ->
                let buffPush = common.uniqueVar(Some("buffPush"))
                let curPush = common.uniqueVar(Some("cursorPush"))
                let stopPush = common.uniqueVar(Some("stopPush"))
                // UGH I need a pure array type declaration that acts like a pointer
                // or need to lower this later
                // First, save the current state
                let save =
                       Suite([Rule(ScalarDeclarationAssign({name=sprintf "*%s" (buffPush.name.levels |> String.concat "");t=AST.Int8(false)}, Some(Expr(Variable(builtins.bufferVar)))))
                              Rule(ScalarDeclarationAssign({name=sprintf "%s" (curPush.name.levels |> String.concat "");t=AST.Int8(false)}, Some(Expr(Variable(builtins.cursorVar)))))
                              Rule(ScalarDeclarationAssign({name=sprintf "%s" (stopPush.name.levels |> String.concat "");t=AST.Int8(false)}, Some(Expr(Variable(builtins.stopVar)))))
                       ])
                // Now set to the user-specified ones
                let update =
                       Suite([
                              Rule(Assignment(ScalarLhs(builtins.bufferVar.name), Expr(this.visitExpr buff)))
                              Rule(Assignment(ScalarLhs(builtins.cursorVar.name), Expr(this.visitExpr cur)))
                              Rule(Assignment(ScalarLhs(builtins.stopVar.name), Expr(this.visitExpr stop)))
                       ])
                // save these for the pop
                buffers.Push buffPush
                cursors.Push curPush
                stops.Push stopPush                  
                // and we're good to go
                Suite([save;update])                       
            | AST.Pop ->
                // get the original things to reset
                let buffPop = buffers.Pop()
                let curPop = cursors.Pop()
                let stopPop = stops.Pop()
                // now reset the current state
                Suite([
                    Rule(Assignment(ScalarLhs(builtins.bufferVar.name), Expr(Variable(buffPop))))
                    Rule(Assignment(ScalarLhs(builtins.cursorVar.name), Expr(Variable(curPop))))
                    Rule(Assignment(ScalarLhs(builtins.stopVar.name), Expr(Variable(stopPop))))
                ])
            | _ -> base.visitStmt x

// take any callbacks on the user's side that correspond to internal functions (like peek) and
// lower them/add in appropriate arguments    
type LowerUserCallbacks() =
    inherit ASTVisitor.Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerUserCallbacks"
        LowerUserCallbacks().visitProgram x    
    
    override this.visitCallback x =
        let x = base.visitCallback x
        let name = x.name
        if name = "peek" then
            if not (x.args.Length = 1) then
                failwith (sprintf "Invalid number of args to callback \"peek\". Expected 1, got %d." x.args.Length)
            match (builtins.lookaheadBits x.args[0] "int64") with
                | AST.Callback(c) -> c
                | _ -> failwith ""
        elif name = "exists" then
            // this may have been inserted by us, check the number of args
            if x.args.Length = 1 then
                match (builtins.exists x.args[0]) with
                    | AST.Callback(c) -> c
                    | _ -> failwith ""
            else
                x // inserted by us in the compiler
        elif name = "moreData" then
            if not (x.args.Length = 0) then
                failwith (sprintf "Invalid number of args to callback \"moreData\". Expected 0, got %d." x.args.Length)
            match builtins.moreData() with
                | AST.Callback(c) -> c
                | _ -> failwith ""
        elif name = "curBuffer" then
            if not (x.args.Length = 0) then
                failwith (sprintf "Invalid number of args to callback \"curBuffer\". Expected 0, got %d." x.args.Length)
            match builtins.curBuffer() with
                | AST.Callback(c) -> c
                | _ -> failwith ""
        elif name = "curCursor" then
            if not (x.args.Length = 0) then
                failwith (sprintf "Invalid number of args to callback \"curCursor\". Expected 0, got %d." x.args.Length)
            match builtins.curCursor() with
                | AST.Callback(c) -> c
                | _ -> failwith ""
        elif name = "curStop" then
            if not (x.args.Length = 0) then
                failwith (sprintf "Invalid number of args to callback \"curStop\". Expected 0, got %d." x.args.Length)
            match builtins.curBuffer() with
                | AST.Callback(c) -> c
                | _ -> failwith ""                   
        else
            // and if it's not any internal thing, wrap in a user temporary holder
            if builtins.isInternal name then
                x
            else
                {name="USER"; args=[Callback(x)]}