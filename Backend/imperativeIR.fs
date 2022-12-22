module Backend.imperativeIR

open AST

// IR suitable for codegen
// utilizes some of the components from the AST 

type declaration =
    | ScalarDeclaration of AST.scalarDeclaration
    | ArrayDeclaration of isHeap: bool * string * AST.scalarType * AST.expr    

type stmt =
    | For of induc: string * lower: AST.expr * upper: AST.expr * body: stmt
    | IfElse of cond: AST.expr * tBody: stmt * fBody: option<stmt>
    | Suite of stmt list
    | ExprStmt of AST.expr
    | Declaration of declaration
    | ScalarDeclarationAssign of AST.scalarDeclaration * rhs: option<AST.expr>
    | Assignment of AST.lhs * AST.expr
    
type element =
    | Syntax of string * ast:declaration list * parseBody: stmt
    | Template of string * bindings: AST.binding list * parseBody: stmt
    | Dummy // for dead things
    
type program = Program of element list

type ConstVisitor() =
    
    abstract member visitScalarType : AST.scalarType -> unit
    abstract member visitIdentifier : AST.identifier -> unit
    abstract member visitLiteralType : AST.literalType -> unit
    abstract member visitVariable : AST.variable -> unit
    abstract member visitLiteral : AST.literal -> unit        
    abstract member visitExpr : AST.expr -> unit
    abstract member visitCallback : AST.callback -> unit
    abstract member visitScalarDeclaration : AST.scalarDeclaration -> unit
    abstract member visitDeclaration : declaration -> unit
    abstract member visitLhs : AST.lhs -> unit
    abstract member visitBinding: AST.binding -> unit
    abstract member visitElement : element -> unit
    abstract member visitStmt : stmt -> unit
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
            | AST.Callback(callback) -> this.visitCallback callback
            | AST.ArrRef(id,idx) ->
                this.visitIdentifier id
                this.visitExpr idx
            | AST.Invert(expr) -> this.visitExpr expr
            | AST.BInvert(expr) -> this.visitExpr expr
            | AST.Not(expr) -> this.visitExpr expr
            | AST.Multiplication(exprs) -> exprs |> List.iter this.visitExpr
            | AST.Division(exprs) -> exprs |> List.iter this.visitExpr
            | AST.Addition(exprs) -> exprs |> List.iter this.visitExpr
            | AST.Subtraction(exprs) -> exprs |> List.iter this.visitExpr
            | AST.LeftShift(l,r) ->
                this.visitExpr l
                this.visitExpr r
            | AST.RightShift(l,r) ->
                this.visitExpr l
                this.visitExpr r
            | AST.GreaterThan(_,l,r) ->
               this.visitExpr l
               this.visitExpr r
            | AST.LessThan(_,l,r) ->
               this.visitExpr l
               this.visitExpr r
            | AST.Equals(_,exprs) -> exprs |> List.iter this.visitExpr
            | AST.BAnd(exprs) -> exprs |> List.iter this.visitExpr
            | AST.BOr(exprs) -> exprs |> List.iter this.visitExpr
            | AST.And(exprs) -> exprs |> List.iter this.visitExpr
            | AST.Or(exprs) -> exprs |> List.iter this.visitExpr
            | AST.Variable(var) -> this.visitVariable var
            | AST.Literal(lit) -> this.visitLiteral lit

    default this.visitCallback x =
        x.args |> List.iter this.visitExpr

    default this.visitScalarDeclaration x = this.visitScalarType x.t
    
    default this.visitDeclaration x =
        match x with
            | ScalarDeclaration(d) -> this.visitScalarDeclaration d
            | ArrayDeclaration(_,_,t,e) ->
                this.visitScalarType t
                this.visitExpr e
            
    default this.visitLhs x =
        match x with
            | AST.ScalarLhs(id) -> this.visitIdentifier id
            | AST.ArrayLhs(id, idx) ->
                this.visitIdentifier id
                this.visitExpr idx
    
    default this.visitBinding x =
        match x with
            | AST.ArrayBinding(_,t,_) -> this.visitScalarType t
            | AST.ScalarBinding(_,t,_) -> this.visitScalarType t
                    
    default this.visitElement x =
        match x with
            | Syntax(_, ast, body) ->
                ast |> List.iter this.visitDeclaration
                this.visitStmt body
            | Template(_, bindings, body) ->
                bindings |> List.iter this.visitBinding
                this.visitStmt body
            | Dummy -> ()
                
    default this.visitStmt x =
        match x with
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
            | Suite(stmts) -> stmts |> List.iter this.visitStmt
            | ExprStmt(e) -> this.visitExpr e
            | Declaration(decl) -> this.visitDeclaration decl
            | ScalarDeclarationAssign(l,r) ->
                this.visitScalarDeclaration l
                match r with
                    | Some(r) -> this.visitExpr r
                    | None -> ()
            | Assignment(l,r) ->
                this.visitLhs l
                this.visitExpr r
            
    default this.visitProgram x =
        match x with
            | Program(elems) -> elems |> List.iter this.visitElement
            
type Rebuilder() =
    
    abstract member visitScalarType : AST.scalarType -> AST.scalarType
    abstract member visitIdentifier : AST.identifier -> AST.identifier
    abstract member visitLiteralType : AST.literalType -> AST.literalType
    abstract member visitVariable : AST.variable -> AST.variable
    abstract member visitLiteral : AST.literal -> AST.literal        
    abstract member visitExpr : AST.expr -> AST.expr
    abstract member visitCallback : AST.callback -> AST.callback
    abstract member visitScalarDeclaration : AST.scalarDeclaration -> AST.scalarDeclaration
    abstract member visitDeclaration : declaration -> declaration
    abstract member visitLhs : AST.lhs -> AST.lhs
    abstract member visitBinding: AST.binding -> AST.binding
    abstract member visitElement : element -> element
    abstract member visitStmt : stmt -> stmt
    abstract member visitProgram : program -> program    
    
    default this.visitScalarType x = x
    
    default this.visitIdentifier x = x
    
    default this.visitLiteralType x = x

    default this.visitVariable x = x
        
    default this.visitLiteral x = x
        
    default this.visitExpr x =
        match x with
            | AST.Callback(callback) -> AST.Callback(this.visitCallback callback)
            | AST.ArrRef(id,idx) ->
                AST.ArrRef(
                    this.visitIdentifier id,
                    this.visitExpr idx
                )
            | AST.Invert(expr) -> AST.Invert(this.visitExpr expr)
            | AST.BInvert(expr) -> AST.BInvert(this.visitExpr expr)
            | AST.Not(expr) -> AST.Not(this.visitExpr expr)
            | AST.Multiplication(exprs) -> AST.Multiplication(exprs |> List.map this.visitExpr)
            | AST.Division(exprs) -> AST.Division(exprs |> List.map this.visitExpr)
            | AST.Addition(exprs) -> AST.Addition(exprs |> List.map this.visitExpr)
            | AST.Subtraction(exprs) -> AST.Subtraction(exprs |> List.map this.visitExpr)
            | AST.LeftShift(l,r) ->
                AST.LeftShift(
                    this.visitExpr l,
                    this.visitExpr r
                )
            | AST.RightShift(l,r) ->
                AST.RightShift(
                    this.visitExpr l,
                    this.visitExpr r
                )
            | AST.GreaterThan(eq,l,r) ->
               AST.GreaterThan(
                    eq,
                    this.visitExpr l,
                    this.visitExpr r
               )
            | AST.LessThan(eq,l,r) ->
               AST.GreaterThan(
                   eq,
                   this.visitExpr l,
                   this.visitExpr r
               )
            | AST.Equals(eq,exprs) ->
                AST.Equals(eq, exprs |> List.map this.visitExpr)
            | AST.BAnd(exprs) -> AST.BAnd(exprs |> List.map this.visitExpr)
            | AST.BOr(exprs) -> AST.BOr(exprs |> List.map this.visitExpr)
            | AST.And(exprs) -> AST.And(exprs |> List.map this.visitExpr)
            | AST.Or(exprs) -> AST.Or(exprs |> List.map this.visitExpr)
            | AST.Variable(var) -> AST.Variable(this.visitVariable var)
            | AST.Literal(lit) -> AST.Literal(this.visitLiteral lit)

    default this.visitCallback x =
        {name=x.name;args=x.args |> List.map this.visitExpr}

    default this.visitScalarDeclaration x =
        {name=x.name;t=this.visitScalarType x.t}
    
    default this.visitDeclaration x =
        match x with
            | ScalarDeclaration(d) -> ScalarDeclaration(this.visitScalarDeclaration d)
            | ArrayDeclaration(isHeap,name,t,e) ->
                ArrayDeclaration(
                    isHeap,
                    name,
                    this.visitScalarType t,
                    this.visitExpr e
                )
            
    default this.visitLhs x =
        match x with
            | AST.ScalarLhs(id) -> AST.ScalarLhs(this.visitIdentifier id)
            | AST.ArrayLhs(id, idx) ->
                AST.ArrayLhs(
                    this.visitIdentifier id,
                    this.visitExpr idx
                    )
    
    default this.visitBinding x =
        match x with
            | AST.ArrayBinding(ref,t,name) -> AST.ArrayBinding(ref,this.visitScalarType t, name)
            | AST.ScalarBinding(ref,t,name) -> AST.ArrayBinding(ref, this.visitScalarType t, name)
                    
    default this.visitElement x =
        match x with
            | Syntax(name, ast, body) ->
                Syntax(
                    name,
                    ast |> List.map this.visitDeclaration,
                    this.visitStmt body
                )
            | Template(name, bindings, body) ->
                Template(
                    name,
                    bindings |> List.map this.visitBinding,
                    this.visitStmt body
                    )
            | Dummy -> Dummy
                
    default this.visitStmt x =
        match x with
            | For(induc,lower,upper,body) ->
                For(
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
            | Suite(stmts) -> Suite(stmts |> List.map this.visitStmt)
            | ExprStmt(e) -> ExprStmt(this.visitExpr e)
            | Declaration(decl) -> Declaration(this.visitDeclaration decl)
            | ScalarDeclarationAssign(l,r) ->
                ScalarDeclarationAssign(
                    this.visitScalarDeclaration l,
                    match r with
                        | Some(r) -> Some(this.visitExpr r)
                        | None -> None
                        )
            | Assignment(l,r) ->
                Assignment(
                    this.visitLhs l,
                    this.visitExpr r
                    )
            
    default this.visitProgram x =
        match x with
            | Program(elems) -> elems |> List.map this.visitElement |> Program            
    
type ASTToImperativeIR() =
    
    static member pass x =
        ASTToImperativeIR().visitProgram x
    
    member this.visitScalarType x = x
    
    member this.visitIdentifier x = x
    
    member this.visitVariable x = x
    
    member this.visitLiteral x = x
    
    member this.visitExpr x = x
    
    member this.visitCallback x = x
    
    member this.visitScalarDeclaration x = x
    
    member this.visitArrayDeclaration x =
        match x with
            | AST.Stack(s,t,i) -> ArrayDeclaration(false, s, t, AST.Literal({lit=AST.Decimal; value=sprintf "%d" i}))
            | AST.Heap(s,t,i) -> ArrayDeclaration(false, s, t, i)            
        
    member this.visitAnyDeclaration x =
        match x with
            | AST.ScalarDeclaration(s) -> ScalarDeclaration(this.visitScalarDeclaration s)
            | AST.ArrayDeclaration(s) -> this.visitArrayDeclaration s
            
    member this.visitLhs x = x
    
    member this.visitRhs x =
        match x with
            | AST.ParseBits _
            | AST.ParseBitsAndValidate _
            | AST.ParseElement _
            | AST.ParseLiteral _
            | AST.ParseTemplate _ ->
                failwith "Rhs parser functions should've been lowered already"
            | AST.Expr ex ->
                this.visitExpr ex
                
    member this.visitRule x =
        match x with
            | AST.ScalarDeclarationAssign(s,r) ->
                ScalarDeclarationAssign(this.visitScalarDeclaration s,
                                        match r with | Some(r) -> Some(this.visitRhs r) | None -> None)
            | AST.ArrayDeclarationOnly(a) -> Declaration(this.visitArrayDeclaration a)
            | AST.Assignment(l,r) -> Assignment(this.visitLhs l, this.visitRhs r)
            | AST.Transient r -> ExprStmt(this.visitRhs r)
            
    member this.visitBinding x = x
    
    member this.visitElement x =
        match x with
            | AST.Syntax(name,ast,body) ->
                Syntax(name, ast |> List.map this.visitAnyDeclaration, this.visitStmt body)
            | AST.Template(name,bindings,body) ->
                Template(name, bindings |> List.map this.visitBinding, this.visitStmt body)
            | _ -> Dummy
            
    member this.visitStmt x =
        match x with
            | AST.Rule r -> this.visitRule r
            | AST.For(a,b,c,d) -> For(a,this.visitExpr b, this.visitExpr c, this.visitStmt d)
            | AST.IfElse(a,b,c) ->
                let f =
                    match c with
                        | Some(f) -> Some(this.visitStmt f)
                        | None -> None 
                IfElse(this.visitExpr a, this.visitStmt b, f)
            | AST.Alternate _ -> failwith "Alternate should've been lowered already"
            | AST.Suite(s) -> Suite(s |> List.map this.visitStmt)
            | AST.ExprStmt(ex) -> ExprStmt(this.visitExpr ex)
            | AST.Push _ -> failwith "Push should've been lowered already"
            | AST.Pop -> failwith "Pop should've been lowered already"
            
    member this.visitProgram x =
        match x with AST.Program elements -> Program(elements |> List.map this.visitElement)
               