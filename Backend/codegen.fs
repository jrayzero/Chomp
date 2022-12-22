module Backend.codegen

open AST
open imperativeIR

// take built-in functions and make them match the runtime signature
// (like adding templates and whatnot)
type LowerFunctions() =
    inherit Rebuilder()
    
    static member pass x =
        printfn "==Running pass LowerFunctions"
        LowerFunctions().visitProgram x    
    
    override this.visitCallback x =
        let name = x.name
        if name = builtins.syntaxParseName || name = builtins.templateParseName ||
            name = builtins.lookaheadBitsName || name = builtins.parseBitsName then
            let dummy = match x.args[0] with | AST.Literal(lit) -> lit.value | _ -> failwith ""
            let newName = sprintf "%s<%s>" name
                              (if name = builtins.lookaheadBitsName ||
                                  name = builtins.parseBitsName then sprintf "%s_t" dummy else dummy)
            {name=newName; args=x.args[1..]}
        elif name = "USER" then
            let arg =
                match x.args[0] with
                    | AST.Callback(c) -> c
                    | _ -> failwith ""
            let name = sprintf "user.%s" arg.name
            {name=name; args = arg.args}
        else
            x
            
type CodegenCPP() =
    
    let mutable ind = 0
    
    let types = System.Collections.Generic.HashSet<string>()
    
    let incr() = ind <- ind + 2
    let decr() = ind <- ind - 2
    
    let indent s =
        let mutable spaces = ""
        for i in 0..ind-1 do
            spaces <- spaces + " "
        sprintf "%s%s" spaces s
        
    static member pass x =
        printfn "==Running pass CodegenCPP"
        CodegenCPP().visitProgram x
        
    member this.visitScalarType x =
        match x with
            | AST.Int8(b) -> if b then "int8_t" else "uint8_t"
            | AST.Int16(b) -> if b then "int16_t" else "uint16_t"
            | AST.Int32(b) -> if b then "int32_t" else "uint32_t"
            | AST.Int64(b) -> if b then "int64_t" else "uint64_t"
            | AST.Bool -> "bool"
            | AST.Float32 -> "float"
            | AST.Float64 -> "double"
            | AST.SyntaxRef(s) -> s
            
    member this.visitIdentifier (x: AST.identifier) =
        x.levels |> String.concat "."
        
    member this.visitVariable (x: AST.variable) =
        this.visitIdentifier x.name
        
    member this.visitLiteral (x: AST.literal) =
        match x.lit with
            | AST.Decimal -> x.value
            | AST.Ascii -> sprintf "\"%s\"" x.value
            | _ -> failwith "Hex and Binary literal types should've been lowered."

    member this.visitExpr x =
        match x with
            | AST.Callback(callback) -> this.visitCallback callback
            | AST.ArrRef(id, idx) ->
                sprintf "%s[%s]" (this.visitIdentifier id) (this.visitExpr idx)
            | AST.Invert(ex) -> sprintf "-(%s)" (this.visitExpr ex)
            | AST.BInvert(ex) -> sprintf "~(%s)" (this.visitExpr ex)
            | AST.Not(ex) -> sprintf "!(%s)" (this.visitExpr ex)
            | AST.Multiplication(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " * "
            | AST.Division(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " / "
            | AST.Addition(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " + "
            | AST.Subtraction(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " - "
            | AST.LeftShift(l,r) -> sprintf "(%s) << (%s)" (this.visitExpr l) (this.visitExpr r)
            | AST.RightShift(l,r) -> sprintf "(%s) >> (%s)" (this.visitExpr l) (this.visitExpr r)
            | AST.GreaterThan(eq,l,r) ->
                sprintf "(%s) >%s (%s)" (this.visitExpr l) (if eq then "=" else "") (this.visitExpr r)
            | AST.LessThan(eq,l,r) ->
                sprintf "(%s) %s (%s)" (this.visitExpr l) (if eq then "=" else "") (this.visitExpr r)
            | AST.Equals(eq,exs) ->
                exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat (if eq then " == " else " != ")
            | AST.BAnd(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " & "
            | AST.BOr(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " | "
            | AST.And(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " && "
            | AST.Or(exs) -> exs |> List.map this.visitExpr |> List.map (sprintf "(%s)") |> String.concat " || "
            | AST.Variable(var) -> this.visitVariable var
            | AST.Literal(lit) -> this.visitLiteral lit
            
    member this.visitCallback x =
        sprintf "%s(%s)" x.name (x.args |> List.map this.visitExpr |> String.concat ", ")
        
    member this.visitScalarDeclaration (x: AST.scalarDeclaration) =
        sprintf "%s %s" (this.visitScalarType x.t) x.name
        
    member this.visitDeclaration x =
        match x with
            | ScalarDeclaration(s) -> this.visitScalarDeclaration s
            | ArrayDeclaration(isHeap,name,t,e) ->
                if isHeap then
                    let sz = this.visitExpr e
                    let t = (this.visitScalarType t)
                    sprintf "%s *%s = new %s[%s]" t name t sz 
                else
                    sprintf "%s %s[%s]" (this.visitScalarType t) name (this.visitExpr e)

    member this.visitStmt x =
        match x with
            | For(induc, lower, upper, stride, body) ->
                let l = this.visitExpr lower
                let u = this.visitExpr upper
                let st = this.visitExpr stride
                incr()
                let sbody = this.visitStmt body
                decr()
                let forFooter = indent("}")
                indent(sprintf "for (int64_t %s = %s; %s < %s; %s+=%s) {\n%s%s\n"
                           induc l induc u induc st sbody forFooter)
            | While(cond, body) ->
                let scond = this.visitExpr cond
                incr()
                let sbody = this.visitStmt body
                decr()
                let footer = indent("}")
                indent(sprintf "while (%s) {\n%s%s\n" scond sbody footer) 
            | IfElse(cond,tBody,fBody) ->
                let scond = this.visitExpr cond
                incr()
                let stBody = this.visitStmt tBody
                decr()
                match fBody with
                    | Some(body) ->
                        incr()
                        let sfBody = this.visitStmt body
                        decr()
                        let footer = indent("}")
                        let top = indent(sprintf "if (%s) {\n%s" scond stBody)
                        let bottom = indent(sprintf "} else {\n%s%s" sfBody footer)
                        sprintf "%s%s\n" top bottom
                    | None ->
                        let footer = indent("}")
                        indent(sprintf "if (%s) {\n%s%s\n" scond stBody footer)
            | Suite(stmts) ->
                sprintf "%s" (stmts |> List.map this.visitStmt |> String.concat "")
            | ExprStmt(ex) -> indent(sprintf "%s;\n" (this.visitExpr ex))
            | Declaration(ex) ->
                indent(sprintf "%s;\n" (this.visitDeclaration ex))
            | ScalarDeclarationAssign(lhs,rhs) ->
                let decl = this.visitScalarDeclaration lhs
                match rhs with
                    | Some(ex) -> 
                        let rhsEx = this.visitExpr ex
                        indent(sprintf "%s = %s;\n" decl rhsEx)
                    | None ->
                        indent(sprintf "%s;\n" decl)
            | Assignment(lhs,rhs) ->
                let l = this.visitLhs lhs
                let r = this.visitExpr rhs
                indent(sprintf "%s = %s;\n" l r)
                
    member this.visitLhs x =
        match x with
            | AST.ScalarLhs(id) -> this.visitIdentifier id
            | AST.ArrayLhs(id,idx) -> sprintf "%s[%s]" (this.visitIdentifier id) (this.visitExpr idx)

    member this.visitBinding x =
        match x with
            | AST.ArrayBinding(ref,t,name) ->
                if ref then
                    sprintf "%s *%s" (this.visitScalarType t) name
                else
                    sprintf "const %s *%s" (this.visitScalarType t) name
            | AST.ScalarBinding(ref,t,name) ->
                if ref then
                   sprintf "%s &%s" (this.visitScalarType t) name
                else
                   sprintf "const %s &%s" (this.visitScalarType t) name
                
    member this.visitElement x =
        match x with
            | Syntax(name, ast, body) ->
                types.Add(name) |> ignore
                let header = indent(sprintf "struct %s {\n" name)
                let footer = indent("};\n")
                incr()
                let fields = ast |> List.map this.visitDeclaration |> List.map (fun s -> indent(sprintf "%s;\n" s))
                            |> String.concat ""
                let templateDecl = indent("template <typename USER>\n")                                                
                let bodyDecl = indent(sprintf "void innerParse(USER &user, uint8_t *%s, uint64_t &%s, uint64_t &%s) {\n"
                                          builtins.bufferName builtins.cursorName builtins.stopName)
                let bodyFooter = indent("}\n")
                incr()
                let sbody = this.visitStmt body                                   
                decr()
                let templateStaticDecl = indent("template <typename USER>\n")
                let staticBodyDecl = indent(sprintf "static %s parse(USER &user, uint8_t *%s, uint64_t &%s, uint64_t &%s) {\n"
                                          name builtins.bufferName builtins.cursorName builtins.stopName)
                let staticBodyFooter = indent("}\n")                
                incr()
                let staticBody = indent(sprintf "%s obj; obj.innerParse(user, %s, %s, %s); return std::move(obj);\n"
                                            name builtins.bufferName builtins.cursorName builtins.stopName)
                decr()                
                decr()
                let staticPart = sprintf "%s%s%s%s" templateStaticDecl staticBodyDecl staticBody staticBodyFooter
                sprintf "%s%s%s%s%s%s%s%s" header fields staticPart templateDecl bodyDecl sbody bodyFooter footer
            | Template(name, bindings, body) ->
                types.Add(name) |> ignore
                let header = indent(sprintf "struct %s {\n" name)
                let footer = indent("};\n")
                let bindings = bindings |> List.map this.visitBinding |> String.concat "," 
                incr()
                // I'm punting type stuff right now, so this just gets template for the binding types!
                let templateDecl = indent(sprintf "template <typename USER>\n")                    
                let bodyDecl = indent(sprintf "static void parse(USER &user, uint8_t *%s, uint64_t &%s, uint64_t &%s, %s) {\n"
                                          builtins.bufferName builtins.cursorName builtins.stopName bindings)
                let bodyFooter = indent("}\n")
                incr()
                let sbody = this.visitStmt body                                   
                decr()
                decr()
                sprintf "%s%s%s%s%s%s" header templateDecl bodyDecl sbody bodyFooter footer
            | Dummy -> ""
            
    member this.visitProgram x =
        let src = match x with Program(p) -> p |> List.map this.visitElement |> String.concat "\n"
        let mutable fwds = ""
        for t in types do
            fwds <- sprintf "%s\nstruct %s;" fwds t
        let header = sprintf "// -*-c++-*-\n\
                      #include <vector>\n\
                      #include \"runtime/cpp/runtime.h\"\n\
                      %s\n" fwds        
        sprintf "%s%s" header src