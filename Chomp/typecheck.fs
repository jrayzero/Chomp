module Chomp.typecheck

open visitor
open AST
open System.Collections.Generic

// ----------------
// General LValues
// ----------------
// - Scalar lvalues can be local or ast values
// - Single level names refer to local or ast values within the current element
// - Multi-level names refer to a syntax element within the current element
// - Array lvalues must be an array access--you can't overwrite arrays

// -------------------------
// General RValues variables
// -------------------------
// - If a term in an arithmetic expression, must be a scalar or an array access
// - If an argument to a function call, can basically be anything
// - If an argument into a binding and that binding is a reference, it must be a variable representing
// a scalar, array (not an array access), or a syntax element 


// ----------------
// Templates
// ----------------
// Bindings
//   - can either be references (&var) or non-references (var)
//   - references can refer to local/ast scalars,arrays,syntax elements variables from calling element
//   - non-references can be the same types as references, or arbitrary expressions
// Array decls
//   - cannot be AST
// LValues
//   - cannot be AST
// Parsing objects
//   - cannot parse a syntax element
// General
//   - binding names, locals, and transients cannot have the same name

type typedVariableKind =
    | ScalarType
    | ArrayType
    | SyntaxType
    | TemplateType
    | Unresolved
    
type typedVariable = {name:string list; kind: typedVariableKind}

type typedTemplate =
    {
      // These all use fully-qualified names
      name: string
      // binding name -> binding name * isScalar * isRef
      bindings: Dictionary<string, string * bool * bool>
      // arr name (all local)
      arrDecls: HashSet<string>
      // lvalue: val name -> val name * isArr
      localLVals: Dictionary<string,string*bool>
      // any vars indexed like an array on
      indexVars: HashSet<string>
      // any un-indexed vars part of an arithmetic expr (like a + b)
      arithmeticVars: HashSet<string>
      // just all used vars
      allVars: HashSet<string>
      // anything used with a parseElement
      parsedElements: HashSet<string>
      }
    
type typedSyntax =
    {
      // These all use fully-qualified names
      name: string
      // arr name (all local)
      arrDecls: HashSet<string>
      // lvalue: val name -> val name * isArr
      localLVals: Dictionary<string,string*bool>
      // any vars indexed like an array
      indexVars: HashSet<string>
      // any un-indexed vars part of an arithmetic expr (like a + b)
      arithmeticVars: HashSet<string>
      // any constant markers
      markerConstants: HashSet<string>
      // just all used vars
      allVars: HashSet<string>
      // anything used with a parseElement
      parsedElements: HashSet<string>
      }
    
type typedConstant =
    {
        constants: HashSet<string>
    }
    
// TODO
// check lvalues/bindings against constants to make sure no conflicting names (and you can't assign to them!)
// check types of bindings against arguments to the template
// check variables exist at some level!
// arithmeticVars don't contain any array vars (b/c that means they are unindexed!)
// parsed elems/constant markers exist

type LOTLConstants() =
    inherit ConstVisitor()
    
    let constants = HashSet<string>()
    
    member this.run x =
        this.visitProgram x
        let newConstants = HashSet<string>()
        for c in constants do newConstants.Add c |> ignore
        {constants = newConstants}
        
    override this.visitElement x =
        match x with
            | Constant(name,_) ->
                if not (constants.Add name) then
                    failwith (sprintf "Found duplicate constant name \"%s\"" name)
            | _ -> ()
    
// Get the lay of the land for everything, but only check minimal validity
// Look at the errors it throws below to see what it checks
type LOTLTemplates() =
    inherit ConstVisitor()
        
    // all templates
    let allNames = HashSet<string>()
    let arithStack = Stack<bool>() // true = in arith expr, false = not in arith expr
    
    // things for a single template
    let mutable templateName = ""
    let bindings = Dictionary<string,string * bool * bool>()
    let localLVals = Dictionary<string,string*bool>()
    let indexVars = HashSet<string>()
    let arithmeticVars = HashSet<string>()
    let parsedElements = HashSet<string>()
    let typedTemplates = List<typedTemplate>()
    let arrDecls = HashSet<string>()
    let allVars = HashSet<string>()

    member this.run x =
        this.visitProgram x
        typedTemplates
    
    override this.visitProgram x =
        arithStack.Push false
        base.visitProgram x
        arithStack.Pop() |> ignore
        assert(arithStack.Count = 0)
        
    override this.visitVariable x =
        allVars.Add (x.name.levels |> String.concat ".") |> ignore
        if arithStack.Peek() then
            arithmeticVars.Add (x.name.levels |> String.concat ".") |> ignore
    
    override this.visitExpr x =
        match x with
            | ArrRef(var, idx) ->
                // not unindexed, so don't add to arithmeticVar!
                indexVars.Add (var.name.levels |> String.concat ".") |> ignore
                allVars.Add (var.name.levels |> String.concat ".") |> ignore
                // restarts expr
                arithStack.Push false
                this.visitExpr idx
                arithStack.Pop() |> ignore
            | Callback _ ->
                // restarts expr
                arithStack.Push false
                base.visitExpr x
                arithStack.Pop() |> ignore
            | Variable(var) ->
                // don't push arithStack b/c you want to know if this within an arithmetic expression or by itself 
                this.visitVariable var
            | _ ->
                arithStack.Push true
                base.visitExpr x
                arithStack.Pop() |> ignore
    
    override this.visitElement x =
        match x with
            | Template(i,b,arrs,body) ->
                templateName <- i.levels |> String.concat "."
                if i.levels.Length > 1 then
                    failwith (sprintf "Cannot have compound name for template definition (got \"%s\")." templateName)
                if not (allNames.Add templateName) then
                    failwith (sprintf "Multiplate Templates found with same name (got \"%s\")." templateName)
                bindings.Clear()
                arrDecls.Clear()
                localLVals.Clear()
                indexVars.Clear()
                arithmeticVars.Clear()
                parsedElements.Clear()
                b |> List.iter this.visitBinding
                arrs |> List.iter this.visitArrDecl
                this.visitStmt body
                // what is the F# way to copy these?
                let newBindings = Dictionary<string, string * bool * bool>()
                let newArrDecls = HashSet<string>()
                let newLocalLVals = Dictionary<string,string*bool>()
                let newIndexVars = HashSet<string>()
                let newArithmeticVars = HashSet<string>()
                let newParsedElements = HashSet<string>()
                let newAllVars = HashSet<string>()
                for KeyValue(k,v) in bindings do
                    newBindings[k] <- v
                for a in arrDecls do newArrDecls.Add(a) |> ignore
                for KeyValue(k,v) in localLVals do
                    newLocalLVals[k] <- v
                for a in indexVars do newIndexVars.Add(a) |> ignore
                for a in arithmeticVars do newArithmeticVars.Add(a) |> ignore
                for a in parsedElements do newParsedElements.Add(a) |> ignore
                for a in allVars do newAllVars.Add(a) |> ignore
                typedTemplates.Add {name=templateName
                                         bindings=newBindings
                                         arrDecls=newArrDecls
                                         localLVals=newLocalLVals
                                         indexVars=newIndexVars
                                         arithmeticVars=newArithmeticVars
                                         allVars=newAllVars
                                         parsedElements=newParsedElements}
            | _ -> () // just want template for now
            
    override this.visitBinding x =
        match x with
            | ArrayBinding(ref,n) ->
                if bindings.ContainsKey n then
                    failwith (sprintf "Found duplicate binding \"%s\" in Template \"%s\"" n templateName)
                bindings[n] <- (n,false,ref)
            | ScalarBinding(ref,n) ->
                if bindings.ContainsKey n then
                    failwith (sprintf "Found duplicate binding \"%s\" in Template \"%s\"" n templateName)                    
                bindings[n] <- (n,true,ref)
                
    override this.visitArrDecl x =
        let name = 
            match x with
                | Stack(isAST,name,_,_) ->
                    if isAST then
                        failwith (sprintf "AST declarations not allowed in Templates (got array \"%s\" in Template \"%s\")." name templateName)
                    name        
                | Heap(isAST,name,_) ->
                    if isAST then
                        failwith (sprintf "AST declarations not allowed in Templates (got array \"%s\" in Template \"%s\")." name templateName)
                    name
        if not (arrDecls.Add name) then
            failwith (sprintf "Found duplicate array declaration \"%s\" in Template \"%s\"" name templateName)
            
    override this.visitRule x =
        match x with
            | TransientLValue r ->
                this.visitParsingRvalue r
            | _ -> base.visitRule x
            
    override this.visitParsingRvalue x =
        match x with
            | ParseElement elem ->
                parsedElements.Add (elem.levels |> String.concat ".") |> ignore // can't check this now since may not have loaded all the templates/elems/constants yet
            | _ -> base.visitParsingRvalue x
            
    override this.visitLvalue x =
        match x with
            | ScalarL(isAST, id) ->
                let name = id.levels |> String.concat "."
                if isAST then
                    failwith (sprintf "AST declarations not allowed in Templates (got scalar \"%s\" in Template \"%s\")." name templateName)
                // Check whether this variable is actually supposed to be an array                    
                if arrDecls.Contains name then
                    failwith (sprintf "Array variable \"%s\" used as scalar in lvalue assignment in Template \"%s\"" name templateName)
                // Check whether this variable is a binding                      
                if bindings.ContainsKey name then
                    match bindings[name] with
                        | bname,isScalar,isRef ->
                            // check whether this binding is actually supposed to be an array
                            if not isScalar then
                                failwith (sprintf "Array binding \"%s\" used as scalar in lvalue assignment in Template \"%s\"" bname templateName)
                            // check whether we are allowed to assign to this binding    
                            if not isRef then
                                failwith (sprintf "Scalar binding \"%s\" assigned to in Template \"%s\", but is not a reference." bname templateName)
                localLVals[name] <- (name, false)    
            | ArrL(id, idx) ->
                let name = id.levels |> String.concat "."
                // check whether this is a binding
                if bindings.ContainsKey name then
                    match bindings[name] with
                        | bname,isScalar,isRef ->
                            // check whether this binding is actually supposed to be a scalar
                            if isScalar then
                                failwith (sprintf "Scalar binding \"%s\" used as array in lvalue assignment in Template \"%s\"" bname templateName)
                            if not isRef then
                                 failwith (sprintf "Array binding \"%s\" assigned to in Template \"%s\", but is not a reference." bname templateName)
                localLVals[name] <- (name, true)
                indexVars.Add (name) |> ignore
                allVars.Add (name) |> ignore
                // idx is basically an rvalue, so process it to fill up things
                this.visitExpr idx
                
type LOTLSyntaxes() =
    inherit ConstVisitor()
        
    // all templates
    let allNames = HashSet<string>()
    let arithStack = Stack<bool>() // true = in arith expr, false = not in arith expr
    
    // things for a single template
    let mutable syntaxName = ""
    let localLVals = Dictionary<string,string*bool>()
    let indexVars = HashSet<string>()
    let arithmeticVars = HashSet<string>()
    let parsedElements = HashSet<string>()
    let typedSyntaxes = List<typedSyntax>()
    let arrDecls = HashSet<string>()
    let allVars = HashSet<string>()
    let markerConstants = HashSet<string>()

    member this.run x =
        this.visitProgram x
        typedSyntaxes
    
    override this.visitMarker x =
        match x with
            | ConstantMarker cons -> markerConstants.Add(cons) |> ignore
            | _ -> ()
    
    override this.visitProgram x =
        arithStack.Push false
        base.visitProgram x
        arithStack.Pop() |> ignore
        assert(arithStack.Count = 0)
        
    override this.visitVariable x =
        allVars.Add (x.name.levels |> String.concat ".") |> ignore
        if arithStack.Peek() then
            arithmeticVars.Add (x.name.levels |> String.concat ".") |> ignore
    
    override this.visitExpr x =
        match x with
            | ArrRef(var, idx) ->
                allVars.Add (var.name.levels |> String.concat ".") |> ignore                
                // not unindexed, so don't add to arithmeticVar!
                indexVars.Add (var.name.levels |> String.concat ".") |> ignore
                // restarts expr
                arithStack.Push false
                this.visitExpr idx
                arithStack.Pop() |> ignore
            | Callback _ ->
                // restarts expr
                arithStack.Push false
                base.visitExpr x
                arithStack.Pop() |> ignore
            | Variable(var) ->
                // don't push arithStack b/c you want to know if this within an arithmetic expression or by itself 
                this.visitVariable var                
            | _ ->
                arithStack.Push true
                base.visitExpr x
                arithStack.Pop() |> ignore
    
    override this.visitElement x =
        match x with
            | Syntax(i,arrs,body) ->
                syntaxName <- i.levels |> String.concat "."
                if i.levels.Length > 1 then
                    failwith (sprintf "Cannot have compound name for template definition (got \"%s\")." syntaxName)
                if not (allNames.Add syntaxName) then
                    failwith (sprintf "Multiplate Syntaxes found with same name (got \"%s\")." syntaxName)
                arrDecls.Clear()
                localLVals.Clear()
                indexVars.Clear()
                arithmeticVars.Clear()
                parsedElements.Clear()
                arrs |> List.iter this.visitArrDecl
                this.visitStmt body
                // what is the F# way to copy these?
                let newArrDecls = HashSet<string>()
                let newLocalLVals = Dictionary<string,string*bool>()
                let newIndexVars = HashSet<string>()
                let newArithmeticVars = HashSet<string>()
                let newParsedElements = HashSet<string>()
                let newAllVars = HashSet<string>()
                let newMarkerConstants = HashSet<string>()
                for a in arrDecls do newArrDecls.Add(a) |> ignore
                for KeyValue(k,v) in localLVals do
                    newLocalLVals[k] <- v
                for a in indexVars do newIndexVars.Add(a) |> ignore
                for a in arithmeticVars do newArithmeticVars.Add(a) |> ignore
                for a in parsedElements do newParsedElements.Add(a) |> ignore
                for a in allVars do newAllVars.Add(a) |> ignore
                for a in markerConstants do newMarkerConstants.Add(a) |> ignore
                typedSyntaxes.Add {name=syntaxName
                                   arrDecls=newArrDecls
                                   localLVals=newLocalLVals
                                   indexVars=newIndexVars
                                   arithmeticVars=newArithmeticVars
                                   markerConstants=newMarkerConstants
                                   allVars=newAllVars
                                   parsedElements=newParsedElements
                                   }
            | _ -> () // just want template for now
            
    override this.visitArrDecl x =
        let name = 
            match x with
                | Stack(isAST,name,_,_) ->
                    name        
                | Heap(isAST,name,_) ->
                    name
        if not (arrDecls.Add name) then
            failwith (sprintf "Found duplicate array declaration \"%s\" in Syntax \"%s\"" name syntaxName)
            
    override this.visitRule x =
        match x with
            | TransientLValue r ->
                this.visitParsingRvalue r
            | _ -> base.visitRule x
            
    override this.visitParsingRvalue x =
        match x with
            | ParseElement elem ->
                parsedElements.Add (elem.levels |> String.concat ".") |> ignore // can't check this now since may not have loaded all the templates/elems/constants yet
            | _ -> base.visitParsingRvalue x
            
    override this.visitLvalue x =
        match x with
            | ScalarL(_, id) ->
                let name = id.levels |> String.concat "."
                // Check whether this variable is actually supposed to be an array                    
                if arrDecls.Contains name then
                    failwith (sprintf "Array variable \"%s\" used as scalar in lvalue assignment in Syntax \"%s\"" name syntaxName)
                localLVals[name] <- (name, false)    
            | ArrL(id, idx) ->
                let name = id.levels |> String.concat "."
                // check that this array was declared or is a binding
                localLVals[name] <- (name, true)
                indexVars.Add (name) |> ignore
                allVars.Add (name) |> ignore
                // idx is basically an rvalue, so process it to fill up things
                this.visitExpr idx
                
type Typecheck() =
    inherit ConstVisitor()
    
    static member pass program =
        printfn "==Running pass LOTLConstants"
        let typedConstants = LOTLConstants().run program
        printfn "%A" typedConstants
        printfn "==Running pass LOTLTemplates"
        let typedTemplates = LOTLTemplates().run program
        printfn "%A" typedTemplates
        printfn "==Running pass LOTLSyntaxes"
        let typedSyntaxes = LOTLSyntaxes().run program
        printfn "%A" typedSyntaxes        
        ()