module Chomp.visitor

open AST

type ConstVisitor() =
    
    abstract member visitScalarType : scalarType -> unit
    abstract member visitIdentifier : identifier -> unit
    abstract member visitLiteralType : literalType -> unit
    abstract member visitExpr : expr -> unit
    abstract member visitLiteral : literal -> unit
    abstract member visitCallback : callback -> unit
    abstract member visitVariable : variable -> unit
    abstract member visitRange : range -> unit
    abstract member visitRule : rule -> unit
    abstract member visitLvalue : lvalue -> unit
    abstract member visitRvalue : rvalue -> unit
    abstract member visitElement : element -> unit
    abstract member visitStmt : stmt -> unit
    abstract member visitMarker : marker -> unit
    abstract member visitArrDecl : arrDecl -> unit
    abstract member visitProgram : program -> unit    
    
    default this.visitScalarType x = ()
    
    default this.visitIdentifier x = ()
    
    default this.visitLiteralType x = ()
    
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
    
    default this.visitLiteral x =
        this.visitLiteralType x.lit
        
    
    default this.visitCallback x =
        this.visitIdentifier x.name
        x.args |> List.iter this.visitExpr
        
    default this.visitVariable x =
        this.visitIdentifier x.user
        this.visitIdentifier x.fqn
        
    default this.visitRange x = ()
    
    default this.visitRule x =
        match x with
            | PersistentLValue(l,r) ->
                this.visitLvalue l
                this.visitRvalue r
            | TransientLValue(i,r) ->
                this.visitIdentifier i
                this.visitRvalue r
            | BindingLValue(i,r) ->
                this.visitIdentifier i
                this.visitRvalue r
                
    default this.visitLvalue x =
        match x with
            | ScalarL(_,i) -> this.visitIdentifier i
            | ArrL(i,expr) ->
                this.visitIdentifier i
                this.visitExpr expr
                
    default this.visitRvalue x =
        match x with
            | ParseOnly(expr) -> this.visitExpr expr
            | ParseAndValidate(expr,r) ->
                this.visitExpr expr
                r |> List.iter this.visitRange
            | Expr(expr) -> this.visitExpr expr
                
    default this.visitElement x =
        match x with
            | Syntax(i, arr, body) ->
                this.visitIdentifier i
                arr |> List.iter this.visitArrDecl
                this.visitStmt body
            | Template(i, bindings, arr, body) ->
                this.visitIdentifier i
                bindings |> List.iter this.visitIdentifier
                arr |> List.iter this.visitArrDecl
                this.visitStmt body
            | Constant(i,lit) ->
                this.visitIdentifier i
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
            | ConstantMarker(con) -> this.visitVariable con
            
    default this.visitArrDecl x =
        match x with
            | Stack(_, i, st, _) ->
                this.visitIdentifier i
                this.visitScalarType st
            | Heap(_, i, st) ->
                this.visitIdentifier i
                this.visitScalarType st
                
    default this.visitProgram x =
        match x with
            | Program(elems) -> elems |> List.iter this.visitElement
                
type Rebuilder() =
    
    abstract member visitScalarType : scalarType -> scalarType
    abstract member visitIdentifier : identifier -> identifier
    abstract member visitLiteralType : literalType -> literalType
    abstract member visitExpr : expr -> expr
    abstract member visitLiteral : literal -> literal
    abstract member visitCallback : callback -> callback
    abstract member visitVariable : variable -> variable
    abstract member visitRange : range -> range
    abstract member visitRule : rule -> rule
    abstract member visitLvalue : lvalue -> lvalue
    abstract member visitRvalue : rvalue -> rvalue
    abstract member visitElement : element -> element
    abstract member visitStmt : stmt -> stmt
    abstract member visitMarker : marker -> marker
    abstract member visitArrDecl : arrDecl -> arrDecl
    abstract member visitProgram : program -> program    
    
    default this.visitScalarType x =
        match x with
            | Int8(s) -> Int8(s)
            | Int16(s) -> Int16(s)
            | Int32(s) -> Int32(s)
            | Int64(s) -> Int64(s)
            | Float32 -> Float32
            | Float64 -> Float64
    
    default this.visitIdentifier x = {levels=x.levels}
     
    default this.visitLiteralType x =
        match x with
            | Hex -> Hex
            | Decimal -> Decimal
            | Binary -> Binary
            | Ascii -> Ascii
    
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
    
    default this.visitLiteral x =
        {lit=this.visitLiteralType x.lit; value=x.value}
    
    default this.visitCallback x =
        {name=this.visitIdentifier x.name; args=x.args |> List.map this.visitExpr}
        
    default this.visitVariable x =
        {user=this.visitIdentifier x.user; fqn=this.visitIdentifier x.fqn}
        
    default this.visitRange x =
        match x with
            | Single(v) -> Single(v)
            | Lower(v) -> Lower(v)
            | Upper(v) -> Upper(v)
            | Range(l,u) -> Range(l,u)
    
    default this.visitRule x =
        match x with
            | PersistentLValue(l,r) ->
                PersistentLValue(
                    this.visitLvalue l,
                    this.visitRvalue r
                    )
            | TransientLValue(i,r) ->
                TransientLValue(
                    this.visitIdentifier i,
                    this.visitRvalue r
                    )
            | BindingLValue(i,r) ->
                BindingLValue(
                    this.visitIdentifier i,
                    this.visitRvalue r
                    )
                
    default this.visitLvalue x =
        match x with
            | ScalarL(a,i) -> ScalarL(a,this.visitIdentifier i)
            | ArrL(i,expr) ->
                ArrL(
                    this.visitIdentifier i,
                    this.visitExpr expr
                    )
                
    default this.visitRvalue x =
        match x with
            | ParseOnly(expr) -> ParseOnly(this.visitExpr expr)
            | ParseAndValidate(expr,r) ->
                ParseAndValidate(
                    this.visitExpr expr,
                    r |> List.map this.visitRange
                    )
            | Expr(expr) -> Expr(this.visitExpr expr)
                
    default this.visitElement x =
        match x with
            | Syntax(i, arr, body) ->
                Syntax(
                    this.visitIdentifier i,
                    arr |> List.map this.visitArrDecl,
                    this.visitStmt body
                    )
            | Template(i, bindings, arr, body) ->
                Template(
                    this.visitIdentifier i,
                    bindings |> List.map this.visitIdentifier,
                    arr |> List.map this.visitArrDecl,
                    this.visitStmt body
                    )
            | Constant(i,lit) ->
                Constant(
                    this.visitIdentifier i,
                    this.visitLiteral lit
                    )
                
    default this.visitStmt x =
        match x with
            | Rule(rule) -> Rule(this.visitRule rule)
            | For(induc,lower,upper,body) ->
                For(
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
            | ConstantMarker(con) -> ConstantMarker(this.visitVariable con)
            
    default this.visitArrDecl x =
        match x with
            | Stack(a, i, st, sz) ->
                Stack(
                    a,
                    this.visitIdentifier i,
                    this.visitScalarType st,
                    sz
                    )
            | Heap(a, i, st) ->
                Heap(a,
                    this.visitIdentifier i,
                    this.visitScalarType st
                    )
                
    default this.visitProgram x =
        match x with
            | Program(elems) -> Program(elems |> List.map this.visitElement)