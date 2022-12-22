module AST.typecheck

// TODO check that nested fields exist (and that they are scalar/array)
// TODO check that the appropriate types of fields are passed into a called template
open AST
open System.Collections.Generic

type TypeCheck() =
    inherit ASTVisitor.ConstVisitor()
    
    // all constant name
    let constants = HashSet<string>()
    
    // syntax name -> ast entry name, type * isArr
    let syntaxAST = Dictionary<string,Dictionary<string,scalarType*bool>>()
    
    // template names -> bindings (bool=isRef,bool=isArr)
    let templates = Dictionary<string,Dictionary<string,scalarType * bool * bool>>()
    
    // nested syntax ast access
    // element name -> identifier * isArr * isLhs
    let deferredAST = Dictionary<string,List<string list * bool * bool>>()
    
    // check that these names are either templates or constants groups
    let deferredParses = HashSet<string>()
    // check that these names are templates
    let deferredTemplates = HashSet<string>()
    // check that these names are constants
    let deferredConstants = HashSet<string>()
    
    // true = template, false = syntax
    let mutable isTemplate = true
    let mutable curElement = ""
    
    // var name -> type * is ref * is arr
    let curDecls = List<Dictionary<string,scalarType*bool*bool>>()
    
    static member pass x =
        printfn "==Running pass TypeCheck"
        TypeCheck().run x
        
    member this.run x = 
        this.visitProgram x
        // check constants
        for constant in deferredConstants do
            if not (constants.Contains constant) then
                failwith (sprintf "Constant \"%s\" not found." constant)
        // check templates
        for template in deferredTemplates do
            if not (templates.ContainsKey template) then
                failwith (sprintf "Template \"%s\" not found." template)
        // check syntax/constants
        for ts in deferredParses do
            if not (constants.Contains ts) && not (syntaxAST.ContainsKey ts) then
               failwith (sprintf "Syntax/Constant \"%s\" not found." ts)
        // check that ast fields exist
        // for KeyValue(elem,items) in deferredAST do
        //     let mutable level = items[0]
        //     for i in 1..items.Count - 1 do
        //         
        //         ()
    
    override this.visitExpr x =
        match x with
            | ArrRef(id,_) ->
                // first check that the base of this exists
                let primaryName = id.levels[0]
                let mutable found = false
                for curDecl in curDecls do
                    if curDecl.ContainsKey primaryName then
                        found <- true
                if not found then
                    failwith (sprintf "Cannot find declaration for array ref \"%s\" in %s \"%s\""
                                  primaryName (if isTemplate then "Template" else "Syntax") curElement)
                if id.levels.Length = 1 then
                    // check it is an array
                    for i in curDecls.Count - 1..-1..0 do
                        if curDecls[i].ContainsKey primaryName then
                            // okay, it's here. check the type
                            let _,_,isArr = curDecls[i][primaryName]
                            if not isArr then
                                failwith (sprintf "Array access to non-array \"%s\" found in %s \"%s\"."
                                              primaryName (if isTemplate then "Template" else "Syntax") curElement)
                else
                    // defer to check later
                    deferredAST[curElement].Add(id.levels,true,false)
                base.visitExpr x
            | _ -> base.visitExpr x
            
    override this.visitRhs x =
        match x with
            | ParseElement(s) -> deferredParses.Add(s) |> ignore
            | ParseTemplate(t,exprs) ->
                deferredTemplates.Add(t) |> ignore
                // TODO verify that the exprs are valid types!
            | _ -> ()
        base.visitRhs x    
            
    override this.visitRule x =
        match x with
            | Assignment(l,r) ->
                match l with
                    | ScalarLhs(id) ->
                        // check that base exists
                        let primaryName = id.levels[0]
                        // just check if declared anywhere
                        let mutable found = false
                        for curDecl in curDecls do
                            if curDecl.ContainsKey primaryName then
                                found <- true
                        if not found then
                            failwith (sprintf "Cannot find declaration for variable \"%s\" in %s \"%s\""
                                        primaryName (if isTemplate then "Template" else "Syntax") curElement)
                        // if it's a binding in a template, make sure it can be written to
                        if isTemplate && templates[curElement].ContainsKey primaryName then
                            let _,isRef,_ = templates[curElement][primaryName]
                            if not isRef then
                                failwith (sprintf "Write to non-ref binding \"%s\" found in %s \"%s\"."
                                              primaryName  (if isTemplate then "Template" else "Syntax") curElement)
                        if id.levels.Length = 1 then
                            // check it is a scalar
                            for i in curDecls.Count - 1..-1..0 do
                                if curDecls[i].ContainsKey primaryName then
                                    // okay, it's here. check the type
                                    let _,_,isArr = curDecls[i][primaryName]
                                    if isArr then
                                        failwith (sprintf "Scalar access to array \"%s\" found in %s \"%s\"."
                                                      primaryName (if isTemplate then "Template" else "Syntax") curElement)
                        else
                            // defer to check later
                            deferredAST[curElement].Add(id.levels,false,true)
                    | ArrayLhs(id,idx) ->
                        // check that base exists
                        let primaryName = id.levels[0]
                        // just check if declared anywhere
                        let mutable found = false
                        for curDecl in curDecls do
                            if curDecl.ContainsKey primaryName then
                                found <- true
                        if not found then
                            failwith (sprintf "Cannot find declaration for variable \"%s\" in %s \"%s\""
                                        primaryName (if isTemplate then "Template" else "Syntax") curElement)
                        // if it's a binding in a template, make sure it can be written to
                        if isTemplate then
                            let _,isRef,_ = templates[curElement][primaryName]
                            if not isRef then
                                failwith (sprintf "Write to non-ref binding \"%s\" found in %s \"%s\"."
                                              primaryName  (if isTemplate then "Template" else "Syntax") curElement)                            
                        if id.levels.Length = 1 then
                            // check it is a scalar
                            for i in curDecls.Count - 1..-1..0 do 
                                if curDecls[i].ContainsKey primaryName then
                                    // okay, it's here. check the type
                                    let _,_,isArr = curDecls[i][primaryName]
                                    if not isArr then
                                        failwith (sprintf "Array access to non-array \"%s\" found in %s \"%s\"."
                                                      primaryName (if isTemplate then "Template" else "Syntax") curElement)
                        else
                            // defer to check later
                            deferredAST[curElement].Add(id.levels,false,true)                        
                        this.visitExpr idx
                this.visitRhs r    
            | _ -> base.visitRule x
            
    override this.visitMarker x =
        match x with
            | ConstantMarker(m) -> deferredConstants.Add(m) |> ignore
            | _ -> ()
    
    override this.visitStmt x =
        match x with
            | For(induc,_,_,_) ->
                curDecls.Add(Dictionary<string,scalarType*bool*bool>())
                curDecls[curDecls.Count - 1][induc] <- (Int64(true), false, false)
                base.visitStmt x
                curDecls.RemoveAt(curDecls.Count - 1)
            | IfElse(cond, tbody, fbody) ->
                this.visitExpr cond
                curDecls.Add (Dictionary<string,scalarType*bool*bool>())
                this.visitStmt tbody
                curDecls.RemoveAt (curDecls.Count-1)
                match fbody with
                    | Some(f) ->
                        curDecls.Add (Dictionary<string,scalarType*bool*bool>())
                        this.visitStmt f
                        curDecls.RemoveAt (curDecls.Count-1)
                    | None -> ()
            | Alternate(objs) ->
                for m,s in objs do
                    this.visitMarker m
                    curDecls.Add (Dictionary<string,scalarType*bool*bool>())
                    this.visitStmt s
                    curDecls.RemoveAt (curDecls.Count-1)
            | _ -> base.visitStmt x
                        
    override this.visitVariable x =
        // variables are part of right hand side expressions
        let primaryName = x.name.levels[0]
        // just check if declared anywhere
        let mutable found = false
        for curDecl in curDecls do
            if curDecl.ContainsKey primaryName then
                found <- true
        if not found then
            failwith (sprintf "Cannot find declaration for variable \"%s\" in %s \"%s\""
                        primaryName (if isTemplate then "Template" else "Syntax") curElement)
        // if this has multiple levels, defer to later
        if x.name.levels.Length > 1 then
            deferredAST[curElement].Add(x.name.levels,false,false)
                
    override this.visitScalarDeclaration x =
        // check that not declared in current scope
        for KeyValue(name,t) in curDecls[curDecls.Count - 1] do
            if name = x.name then
                failwith (sprintf "Scalar variable \"%s\" already declared in current scope for %s \"%s\""
                              name (if isTemplate then "Template" else "Syntax") curElement)
        curDecls[curDecls.Count-1][x.name] <- (x.t,false,false)
            
    override this.visitArrayDeclaration x =
        let aname,t =
            match x with
                | Stack(name,t,_) -> name,t
                | Heap(name,t,_) -> name,t
        // check that not declared in current scope
        for KeyValue(name,t) in curDecls[curDecls.Count - 1] do
            if name = aname then
                failwith (sprintf "Array variable \"%s\" already declared in current scope for %s \"%s\""
                              name (if isTemplate then "Template" else "Syntax") curElement)
        curDecls[curDecls.Count-1][aname] <- (t,false,true)                
    
    override this.visitElement x =
        match x with
            | Syntax(name,ast,body) ->
                curElement <- name
                isTemplate <- false
                deferredAST[curElement] <- List<string list*bool*bool>()
                curDecls.Add (Dictionary<string,scalarType*bool*bool>())
                if syntaxAST.ContainsKey name then
                    failwith (sprintf "Duplicate Syntax name \"%s\"" name)
                if templates.ContainsKey name then                     
                    failwith (sprintf "Name collision between Syntax and Template (got \"%s\")" name)
                if constants.Contains name then
                    failwith (sprintf "Name collision between Syntax and Constant (got \"%s\")" name)
                syntaxAST[name] <- Dictionary<string,scalarType * bool>()
                for a in ast do
                    match a with
                        | ScalarDeclaration(s) ->
                            syntaxAST[name][s.name] <- (s.t, false)
                            curDecls[curDecls.Count - 1][s.name] <- (s.t, true, false)
                        | ArrayDeclaration(a) ->
                            match a with
                                | Stack(n,t,_) ->
                                    syntaxAST[name][n] <- (t, true)
                                    curDecls[curDecls.Count - 1][n] <- (t, true, true)
                                | Heap(n,t,e) ->
                                    syntaxAST[name][n] <- (t, true)
                                    curDecls[curDecls.Count - 1][n] <- (t, true, true)
                                    this.visitExpr e
                
                this.visitStmt body
                curDecls.RemoveAt(curDecls.Count - 1)
            | Template(name,bindings,body) ->
                curElement <- name
                isTemplate <- true
                deferredAST[curElement] <- List<string list*bool*bool>()
                curDecls.Add (Dictionary<string,scalarType*bool*bool>())
                if templates.ContainsKey name then
                    failwith (sprintf "Duplicate template name \"%s\"" name)
                if syntaxAST.ContainsKey name then                     
                    failwith (sprintf "Name collision between Template and Syntax (got \"%s\")" name)
                if constants.Contains name then
                    failwith (sprintf "Name collision between Template and Constant (got \"%s\")" name)
                templates[name] <- Dictionary<string,scalarType * bool * bool>()
                for binding in bindings do
                    match binding with
                        | ArrayBinding(ref,t,bname) ->
                            if templates[name].ContainsKey bname then
                                failwith (sprintf "Duplicate binding \"%s\" in Template \"%s\"" bname name)
                            templates[name][bname] <- (t,ref,true)
                            curDecls[curDecls.Count - 1][bname] <- (t,ref,true)
                        | ScalarBinding(ref,t,bname) ->
                            if templates[name].ContainsKey bname then   
                                failwith (sprintf "Duplicate binding \"%s\" in Template \"%s\"" bname name)                            
                            templates[name][bname] <- (t,ref,false)
                            curDecls[curDecls.Count - 1][bname] <- (t,ref,false)
                curDecls.Add (Dictionary<string,scalarType*bool*bool>()) // body is a new scope from the bindings
                this.visitStmt body
                curDecls.RemoveAt(curDecls.Count - 1)
                curDecls.RemoveAt(curDecls.Count - 1)
            | Constant(name,_) ->
                if not (constants.Add(name)) then
                    failwith (sprintf "Duplicate constant name \"%s\"" name)
                if syntaxAST.ContainsKey name then                     
                    failwith (sprintf "Name collision between Constant and Syntax (got \"%s\")" name)
                if templates.ContainsKey name then                     
                    failwith (sprintf "Name collision between Constant and Syntax (got \"%s\")" name)                      