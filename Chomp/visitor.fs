module Chomp.visitor

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
            | ArrRef(var,idx) ->
                this.visitVariable var
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
                this.visitIdentifier induc
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
            | ArrRef(var,idx) ->
                ArrRef(
                    this.visitVariable var,
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
                    this.visitIdentifier induc,
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
            
    default this.visitMarker x =
        match x with
            | LiteralMarker(lit) -> LiteralMarker(this.visitLiteral lit)
            | ConstantMarker(s) -> ConstantMarker(s)
            
    default this.visitProgram x =
        match x with
            | Program(elems) -> Program(elems |> List.map this.visitElement)            