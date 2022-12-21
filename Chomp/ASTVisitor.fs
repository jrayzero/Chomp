module Chomp.ASTVisitor

open AST
open Chomp.AST

type ConstVisitor() =
    
    abstract member visitScalarType : scalarType -> unit
    abstract member visitIdentifier : identifier -> unit
    abstract member visitLiteralType : literalType -> unit
    abstract member visitVariable : variable -> unit
    abstract member visitLiteral : literal -> unit        
    abstract member visitExpr : expr -> unit
    abstract member visitCallback : callback -> unit
    abstract member visitRange : range -> unit
    abstract member visitScalarDeclaration : scalarDeclaration -> unit
    abstract member visitArrayDeclaration : arrayDeclaration -> unit
    abstract member visitAnyDeclaration : anyDeclaration -> unit
    abstract member visitLhs : lhs -> unit
    abstract member visitRhs : rhs -> unit
    abstract member visitRule : rule -> unit
    abstract member visitBinding: binding -> unit
    abstract member visitElement : element -> unit
    abstract member visitStmt : stmt -> unit
    abstract member visitMarker : marker -> unit
    abstract member visitProgram : program -> unit    
    
    default this.visitScalarType x = ()
    
    default this.visitIdentifier x = ()
    
    default this.visitLiteralType x = ()

    default this.visitVariable x =
        this.visitIdentifier x.name
        
    default this.visitLiteral x =
        this.visitLiteralType x.lit
        
    default this.visitExpr x =
        match x with
            | Callback(callback) -> this.visitCallback callback
            | ArrRef(id,idx) ->
                this.visitIdentifier id
                this.visitExpr idx
            | Invert(expr) -> this.visitExpr expr
            | BInvert(expr) -> this.visitExpr expr
            | Not(expr) -> this.visitExpr expr
            | Multiplication(exprs) -> exprs |> List.iter this.visitExpr
            | Division(exprs) -> exprs |> List.iter this.visitExpr
            | Addition(exprs) -> exprs |> List.iter this.visitExpr
            | Subtraction(exprs) -> exprs |> List.iter this.visitExpr
            | LeftShift(l,r) ->
                this.visitExpr l
                this.visitExpr r
            | RightShift(l,r) ->
                this.visitExpr l
                this.visitExpr r
            | GreaterThan(_,l,r) ->
               this.visitExpr l
               this.visitExpr r
            | LessThan(_,l,r) ->
               this.visitExpr l
               this.visitExpr r
            | Equals(_,exprs) -> exprs |> List.iter this.visitExpr
            | BAnd(exprs) -> exprs |> List.iter this.visitExpr
            | BOr(exprs) -> exprs |> List.iter this.visitExpr
            | And(exprs) -> exprs |> List.iter this.visitExpr
            | Or(exprs) -> exprs |> List.iter this.visitExpr
            | Variable(var) -> this.visitVariable var
            | Literal(lit) -> this.visitLiteral lit

    default this.visitCallback x =
        x.args |> List.iter this.visitExpr

    default this.visitRange x = ()
    
    default this.visitScalarDeclaration x = this.visitScalarType x.t
    
    default this.visitArrayDeclaration x =
        match x with
            | Stack(_,t,_) -> this.visitScalarType t
            | Heap(_,t,ex) ->
                this.visitScalarType t
                this.visitExpr ex
            
    default this.visitAnyDeclaration x =
        match x with
            | ScalarDeclaration(d) -> this.visitScalarDeclaration d
            | ArrayDeclaration(d) -> this.visitArrayDeclaration d
            
    default this.visitLhs x =
        match x with
            | ScalarLhs(id) -> this.visitIdentifier id
            | ArrayLhs(id, idx) ->
                this.visitIdentifier id
                this.visitExpr idx
    
    default this.visitRhs x =
        match x with
            | ParseBits(expr) -> this.visitExpr expr
            | ParseBitsAndValidate(expr,r) ->
                this.visitExpr expr
                r |> List.iter this.visitRange
            | ParseLiteral(l) -> this.visitLiteral l
            | ParseTemplate(_,e) -> e |> List.iter this.visitExpr
            | Expr(expr) -> this.visitExpr expr
            | _ -> ()
    
    default this.visitRule x =
        match x with
            | ScalarDeclarationAssign(d,r) ->
                this.visitScalarDeclaration d
                match r with
                    | Some(s) -> this.visitRhs s
                    | None -> ()
            | ArrayDeclarationOnly(a) -> this.visitArrayDeclaration a        
            | Assignment(l,r) ->
                this.visitLhs l
                this.visitRhs r
            | Transient(r) -> this.visitRhs r
                
    default this.visitBinding x =
        match x with
            | ArrayBinding(_,t,_) -> this.visitScalarType t
            | ScalarBinding(_,t,_) -> this.visitScalarType t
                    
    default this.visitElement x =
        match x with
            | Syntax(_, ast, body) ->
                ast |> List.iter this.visitAnyDeclaration
                this.visitStmt body
            | Template(_, bindings, body) ->
                bindings |> List.iter this.visitBinding
                this.visitStmt body
            | Constant(_,lit) ->
                this.visitLiteral lit
                
    default this.visitStmt x =
        match x with
            | Rule(rule) -> this.visitRule rule
            | For(induc,lower,upper,body) ->
                this.visitExpr lower
                this.visitExpr upper
                this.visitStmt body
            | IfElse(cond, tBody, fBody) ->
                this.visitExpr cond
                this.visitStmt tBody
                match fBody with
                    | Some(f) -> this.visitStmt f
                    | None -> ()
            | Alternate(items) ->
                items |> List.iter (fun(x, y) -> this.visitMarker x; this.visitStmt y)
            | Suite(stmts) -> stmts |> List.iter this.visitStmt
            | Push(buffer,lower,upper) ->
                this.visitExpr buffer
                this.visitExpr lower
                this.visitExpr upper
            | Pop -> ()
            | ExprStmt(e) -> this.visitExpr e
            
    default this.visitMarker x =
        match x with
            | LiteralMarker(lit) -> this.visitLiteral lit
            | _ -> ()
            
    default this.visitProgram x =
        match x with
            | Program(elems) -> elems |> List.iter this.visitElement
                
type Rebuilder() =
    
    abstract member visitScalarType : scalarType -> scalarType
    abstract member visitIdentifier : identifier -> identifier
    abstract member visitLiteralType : literalType -> literalType
    abstract member visitVariable : variable -> variable
    abstract member visitLiteral : literal -> literal        
    abstract member visitExpr : expr -> expr
    abstract member visitCallback : callback -> callback
    abstract member visitRange : range -> range
    abstract member visitScalarDeclaration : scalarDeclaration -> scalarDeclaration
    abstract member visitArrayDeclaration : arrayDeclaration -> arrayDeclaration
    abstract member visitAnyDeclaration : anyDeclaration -> anyDeclaration
    abstract member visitLhs : lhs -> lhs
    abstract member visitRhs : rhs -> rhs
    abstract member visitRule : rule -> rule
    abstract member visitBinding: binding -> binding
    abstract member visitElement : element -> element
    abstract member visitStmt : stmt -> stmt
    abstract member visitMarker : marker -> marker
    abstract member visitProgram : program -> program    
    
    default this.visitScalarType x = x
    
    default this.visitIdentifier x = {levels=x.levels}
    
    default this.visitLiteralType x = x

    default this.visitVariable x = {name=this.visitIdentifier x.name}
        
    default this.visitLiteral x =
        {lit=this.visitLiteralType x.lit; value=x.value}
        
    default this.visitExpr x =
        match x with
            | Callback(callback) -> Callback(this.visitCallback callback)
            | ArrRef(id,idx) ->
                ArrRef(
                    this.visitIdentifier id,
                    this.visitExpr idx
                    )
            | Invert(expr) -> Invert(this.visitExpr expr)
            | BInvert(expr) -> BInvert(this.visitExpr expr)
            | Not(expr) -> Not(this.visitExpr expr)
            | Multiplication(exprs) -> Multiplication(exprs |> List.map this.visitExpr)
            | Division(exprs) -> Division(exprs |> List.map this.visitExpr)
            | Addition(exprs) -> Addition(exprs |> List.map this.visitExpr)
            | Subtraction(exprs) -> Subtraction(exprs |> List.map this.visitExpr)
            | LeftShift(l,r) ->
                LeftShift(
                    this.visitExpr l,
                    this.visitExpr r
                    )
            | RightShift(l,r) ->
                RightShift(
                    this.visitExpr l,
                    this.visitExpr r
                    )
            | GreaterThan(eq,l,r) ->
               GreaterThan(
                   eq,
                   this.visitExpr l,
                   this.visitExpr r
                   )                
            | LessThan(eq,l,r) ->
               LessThan(
                   eq,
                   this.visitExpr l,
                   this.visitExpr r
                   )
            | Equals(eq,exprs) -> Equals(eq, exprs |> List.map this.visitExpr)
            | BAnd(exprs) -> BAnd(exprs |> List.map this.visitExpr)
            | BOr(exprs) -> BOr(exprs |> List.map this.visitExpr)
            | And(exprs) -> And(exprs |> List.map this.visitExpr)
            | Or(exprs) -> Or(exprs |> List.map this.visitExpr)
            | Variable(var) -> Variable(this.visitVariable var)
            | Literal(lit) -> Literal(this.visitLiteral lit)
            
    default this.visitCallback x =
        {name=x.name; args=x.args |> List.map this.visitExpr}

    default this.visitRange x = x
    
    default this.visitScalarDeclaration x =
        {name=x.name; t=this.visitScalarType x.t}
    
    default this.visitArrayDeclaration x =
        match x with
            | Stack(name,t,sz) ->
                Stack(name, this.visitScalarType t, sz)
            | Heap(name,t,ex) ->
                Heap(name, this.visitScalarType t, this.visitExpr ex)
            
    default this.visitAnyDeclaration x =
        match x with
            | ScalarDeclaration(d) -> ScalarDeclaration(this.visitScalarDeclaration d)
            | ArrayDeclaration(d) -> ArrayDeclaration(this.visitArrayDeclaration d)
            
    default this.visitLhs x =
        match x with
            | ScalarLhs(id) -> ScalarLhs(this.visitIdentifier id)
            | ArrayLhs(id, idx) ->
                ArrayLhs(
                    this.visitIdentifier id,
                    this.visitExpr idx
                )
    
    default this.visitRhs x =
        match x with
            | ParseBits(expr) -> ParseBits(this.visitExpr expr)
            | ParseBitsAndValidate(expr,r) ->
                ParseBitsAndValidate(
                    this.visitExpr expr,
                    r |> List.map this.visitRange
                )
            | ParseElement(s) -> ParseElement(s)
            | ParseLiteral(l) -> ParseLiteral(this.visitLiteral l)
            | ParseTemplate(s,e) -> ParseTemplate(s, e |> List.map this.visitExpr)
            | Expr(expr) -> Expr(this.visitExpr expr)
    
    default this.visitRule x =
        match x with
            | ScalarDeclarationAssign(d,r) ->
                match r with
                    | Some(s) ->
                        ScalarDeclarationAssign(this.visitScalarDeclaration d, Some(this.visitRhs s))
                    | None -> ScalarDeclarationAssign(this.visitScalarDeclaration d, None)
            | ArrayDeclarationOnly(arr) -> ArrayDeclarationOnly(this.visitArrayDeclaration arr)                    
            | Assignment(l,r) ->
                Assignment(
                    this.visitLhs l,
                    this.visitRhs r
                    )
            | Transient(r) -> Transient(this.visitRhs r)
                
    default this.visitBinding x =
        match x with
            | ArrayBinding(r,t,name) -> ArrayBinding(r, this.visitScalarType t, name)
            | ScalarBinding(r,t,name) -> ScalarBinding(r, this.visitScalarType t, name)        
                    
    default this.visitElement x =
        match x with
            | Syntax(name, ast, body) ->
                Syntax(
                    name,
                    ast |> List.map this.visitAnyDeclaration,
                    this.visitStmt body
                    )
            | Template(name, bindings, body) ->
                Template(
                    name,
                    bindings |> List.map this.visitBinding,
                    this.visitStmt body
                    )
            | Constant(name,lit) ->
                Constant(name, this.visitLiteral lit)
                
    default this.visitStmt x =
        match x with
            | Rule(rule) -> Rule(this.visitRule rule)
            | For(induc,lower,upper,body) ->
                For (
                    induc,
                    this.visitExpr lower,
                    this.visitExpr upper,
                    this.visitStmt body
                    )
            | IfElse(cond, tBody, fBody) ->
                IfElse(
                    this.visitExpr cond,
                    this.visitStmt tBody,
                    match fBody with
                        | Some(f) -> Some(this.visitStmt f)
                        | None -> None
                    )
            | Alternate(items) ->
                Alternate(items |> List.map (fun(x, y) -> (this.visitMarker x, this.visitStmt y)))
            | Suite(stmts) -> Suite(stmts |> List.map this.visitStmt)
            | Push(buffer,lower,upper) ->
                Push(
                    this.visitExpr buffer,
                    this.visitExpr lower,
                    this.visitExpr upper
                    )
            | Pop -> Pop
            | ExprStmt(ex) -> ExprStmt(this.visitExpr ex)
            
    default this.visitMarker x =
        match x with
            | LiteralMarker(lit) -> LiteralMarker(this.visitLiteral lit)
            | ConstantMarker(s) -> ConstantMarker(s)
            
    default this.visitProgram x =
        match x with
            | Program(elems) -> Program(elems |> List.map this.visitElement)

// print out chomp code            
type Regenerate() =
    
    let mutable ind = 0
    
    let incr() = ind <- ind + 2
    let decr() = ind <- ind - 2
    
    let indent s =
        let mutable spaces = ""
        for i in 0..ind do
            spaces <- spaces + " "
        sprintf "%s%s" spaces s
    
    member this.visitScalarType x =
        match x with
            | Int8(b) -> if b then "int8" else "uint8"
            | Int16(b) -> if b then "int16" else "uint16"
            | Int32(b) -> if b then "int32" else "uint32"
            | Int64(b) -> if b then "int64" else "uint64"
            | Bool -> "bool"
            | Float32 -> "float32"
            | Float64 -> "float64"
            | SyntaxRef(s) -> s
            
    member this.visitIdentifier x =
        x.levels |> String.concat "."
        
    member this.visitVariable (x:variable) =
        this.visitIdentifier x.name
        
    member this.visitLiteral x =
        match x.lit with
            | Hex -> $"0x{x.value}"
            | Decimal -> x.value
            | Binary -> $"0b{x.value}"
            | Ascii -> sprintf "\"%s\"" x.value
        
    member this.visitExpr x =
        match x with
            | Callback(callback) -> this.visitCallback callback
            | ArrRef(id, idx) ->
                sprintf "%s[%s]" (this.visitIdentifier id) (this.visitExpr idx)
            | Invert(ex) -> sprintf "-(%s)" (this.visitExpr ex)
            | BInvert(ex) -> sprintf "~(%s)" (this.visitExpr ex)
            | Not(ex) -> sprintf "!(%s)" (this.visitExpr ex)
            | Multiplication(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " * "
            | Division(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " / "
            | Addition(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " + "
            | Subtraction(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " - "
            | LeftShift(l,r) -> sprintf "(%s) << (%s)" (this.visitExpr l) (this.visitExpr r)
            | RightShift(l,r) -> sprintf "(%s) >> (%s)" (this.visitExpr l) (this.visitExpr r)
            | GreaterThan(eq,l,r) ->
                sprintf "(%s) >%s (%s)" (this.visitExpr l) (if eq then "=" else "") (this.visitExpr r)
            | LessThan(eq,l,r) ->
                sprintf "(%s) %s (%s)" (this.visitExpr l) (if eq then "=" else "") (this.visitExpr r)
            | Equals(eq,exs) ->
                exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat (if eq then " == " else " != ")
            | BAnd(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " & "
            | BOr(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " | "
            | And(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " && "
            | Or(exs) -> exs |> List.map this.visitExpr |> List.map (fun s -> sprintf "(%s)" s) |> String.concat " || "
            | Variable(var) -> this.visitVariable var
            | Literal(lit) -> this.visitLiteral lit
                
    member this.visitCallback x =
        sprintf "%s(%s)" x.name (x.args |> List.map this.visitExpr |> String.concat ", ")
        
    member this.visitRange x =
        match x with
            | Single(i) -> sprintf "%d" i
            | Lower(i) -> sprintf "%d.." i
            | Upper(i) -> sprintf "..%d" i
            | Range(l,u) -> sprintf "%d..%d" l u
        
    member this.visitScalarDeclaration x =
        sprintf "%s::%s" x.name (this.visitScalarType x.t)
        
    member this.visitArrayDeclaration x =
        match x with
            | Stack(name,t,sz) ->
                sprintf "%s::%s[%d]" name (this.visitScalarType t) sz
            | Heap(name,t,sz) ->
                sprintf "@%s::%s[%s]" name (this.visitScalarType t) (this.visitExpr sz)
        
    member this.visitAnyDeclaration x =
        match x with
            | ScalarDeclaration(scalar) -> this.visitScalarDeclaration scalar
            | ArrayDeclaration(array) -> this.visitArrayDeclaration array
        
    member this.visitLhs x =
        match x with
            | ScalarLhs(id) -> this.visitIdentifier id
            | ArrayLhs(id,idx) -> sprintf "%s[%s]" (this.visitIdentifier id) (this.visitExpr idx)
        
    member this.visitRhs x =
        match x with
            | ParseBits(ex) -> sprintf "[%s]" (this.visitExpr ex)
            | ParseBitsAndValidate(ex,ranges) ->
                sprintf "[%s]{%s}" (this.visitExpr ex) (ranges |> List.map this.visitRange |> String.concat ", ")
            | ParseElement(name) -> sprintf "<%s>" name
            | ParseLiteral(lit) -> sprintf "<%s>" (this.visitLiteral lit)
            | ParseTemplate(name,bindings) ->
                sprintf "<%s(%s)>" name (bindings |> List.map this.visitExpr |> String.concat ", ")
            | Expr(ex) -> this.visitExpr ex
        
    member this.visitRule x =
        match x with
            | ScalarDeclarationAssign(decl,ropt) ->
                let sdecl = this.visitScalarDeclaration decl
                match ropt with
                    | Some(r) -> sprintf "%s := %s" sdecl (this.visitRhs r)
                    | None -> sdecl
            | ArrayDeclarationOnly(decl) -> this.visitArrayDeclaration decl
            | Assignment(l,r) ->
                let sl = this.visitLhs l
                let rl = this.visitRhs r
                sprintf "%s := %s" sl rl
            | Transient(r) -> this.visitRhs r
            
    member this.visitBinding x =
        match x with
            | ArrayBinding(ref,t,name) ->
                let st = sprintf "%s::%s[]" name (this.visitScalarType t)
                if ref then
                    sprintf "&%s" st
                else
                    st
            | ScalarBinding(ref,t,name) ->
                let st = sprintf "%s::%s" name (this.visitScalarType t)
                if ref then
                   sprintf "&%s" st
                else
                    st
        
    member this.visitElement x =
        match x with
            | Syntax(name,ast,body) ->
                incr()
                let astHeader = indent("ast {\n")
                let astFooter = indent("}")
                incr()
                let astMiddle = sprintf "%s" (ast |> List.map (fun y -> indent(sprintf "%s;"
                                                                                   (this.visitAnyDeclaration y)))
                                              |> String.concat "\n")
                decr()
                let sast = sprintf "%s%s%s" astHeader astMiddle astFooter
                let sbody = this.visitStmt body 
                decr()
                sprintf "syntax %s {\n%s\n%s}" name sast sbody
            | Template(name,bindings,body) ->
                let sbindings = bindings |> List.map this.visitBinding |> String.concat ", "
                incr()
                let sbody = this.visitStmt body 
                decr()
                sprintf "template %s (%s) {\n%s}" name sbindings sbody
            | Constant(name,lit) ->
                sprintf "constant %s := %s;" name (this.visitLiteral lit)
        
    member this.visitStmt x =
        match x with
            | Rule(rule) -> indent(sprintf "%s;\n" (this.visitRule rule))
            | For(induc, lower, upper, body) ->
                let l = this.visitExpr lower
                let u = this.visitExpr upper
                incr()
                let sbody = this.visitStmt body
                decr()
                let forFooter = indent("}")
                indent(sprintf "for %s in %s to %s {\n%s%s\n" induc l u sbody forFooter)
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
                        let top = indent(sprintf "if %s {\n%s" scond stBody)
                        let bottom = indent(sprintf "} else {\n%s%s" sfBody footer)
                        sprintf "%s%s\n" top bottom
                    | None ->
                        let footer = indent("}")
                        indent(sprintf "if %s {\n%s%s\n" scond stBody footer)
            | Alternate(options) ->
                incr()
                let processBody s =
                    incr()
                    let ss = this.visitStmt s
                    decr()
                    ss
                let soptions = options |> List.map (fun (x,y) ->
                    let footer = indent("}")
                    let header = indent("marker")
                    sprintf "%s %s {\n%s%s" header (this.visitMarker x) (processBody y) footer)
                               |> String.concat "\n"
                decr()
                let footer = indent("}")
                indent(sprintf "alternate {\n%s%s\n" soptions footer)
            | Suite(stmts) ->
                sprintf "%s" (stmts |> List.map this.visitStmt |> String.concat "")
            | ExprStmt(ex) -> indent(sprintf "%s;\n" (this.visitExpr ex))
            | Push(buff,lower,upper) ->
                indent(sprintf "push %s %s %s;\n" (this.visitExpr buff) (this.visitExpr lower) (this.visitExpr upper))
            | Pop -> indent("pop;\n")
            
    member this.visitMarker x =
        match x with
            | LiteralMarker(lit) -> this.visitLiteral lit
            | ConstantMarker(s) -> s
    
    member this.visitProgram x =
        match x with
            | Program(elems) -> elems |> List.map this.visitElement |> String.concat "\n"