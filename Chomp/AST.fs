module Chomp.AST

type scalarType =
    | Int8 of signed:bool
    | Int16 of signed:bool
    | Int32 of signed:bool
    | Int64 of signed:bool
    | Float32
    | Float64

type identifier = { levels: string list }

type literalType =
    | Hex
    | Decimal
    | Binary
    | Ascii
    
// the numbers represent the precedence (copies from C)
// we only have left-associativity, so we don't need to worry really
// about handling things specially at the same precedence level
type expr =
    // 0
    | Callback of callback // foo(args...)
    | ArrRef of variable * idx: expr // arr[idx]
    // 1
    | Invert of expr // -expr 
    | BInvert of expr // ~expr
    | Not of expr // !expr
    // 2
    | Multiplication of expr list
    | Division of expr list
    // 3
    | Addition of expr list
    | Subtraction of expr list
    // 4
    | LeftShift of expr * expr
    | RightShift of expr * expr
    // 5
    | GreaterThan of bool * expr * expr
    | LessThan of bool * expr * expr
    // 6
    | Equals of bool * expr list
    // 7
    | BAnd of expr list
    // 8
    | BOr of expr list
    // 9
    | And of expr list
    // 10
    | Or of expr list
    // 11
    | Variable of variable
    | Literal of literalType * string
    
and callback = { name: string; args: expr list }

and variable =
    { user: identifier; fqn: identifier; kind: variableKind }
    static member Default user =
        { user = user; fqn = user; kind = Undef }

and variableKind =
    | SyntaxRef
    | LocalRef
    | ASTRef
    | ConstantBindRef
    | TemplateRef
    | Undef // not know yet
    
and range =
    | Single of expr // value
    | Lower of expr // lower..
    | Upper of expr // ..upper
    | Range of lower: expr * upper: expr // lower..upper
    
let EqCurry kind exprs = Equals(kind, exprs)
let LTCurry kind lhs rhs = LessThan(kind, lhs, rhs)
let GTCurry kind lhs rhs = GreaterThan(kind, lhs, rhs)
    
type rule =
    // transients don't have general lvalue since it doesn't make sense for an array to be transient
    | PersistentLValue of lvalue * rvalue // @x/x/@x[idx]/x[idx] := rvalue
    | TransientLValue of identifier * rvalue // !x := rvalue
    | PersistentChoice of lvalue * rvalue list // @x = rvalue0 / rvalue1 / ...
    | TransientChoice of identifier * rvalue list // !x = rvalue0 / rvalue1 / ...
    
and lvalue =
    | ScalarL of isAST: bool * identifier // x or @x
    | ArrL of isAST: bool * identifier * expr // x[idx] or @x[idx]
    
// determines the type of the lvalue     
and rvalue =
    | ParseOnly of scalarType * expr // [20] or [SyntaxGroup/template/constant] if lvalue is a storage, keep the value, otherwise just skip
    | ParseAndValidate of scalarType * expr * rhs: range list // [20=0..2,10,] parse the value and then compare to the rhs possibilities
    | Expr of expr // a + b, lets you just assign to some arbitrary expr
    
type stmt =
    // syntax ident
    // ...arrayDecls...;
    // ...rules...;
    // end
    | SyntaxGroup of identifier * arrDecl list * body: stmt
    // template ident
    // ...arrayDecls...;
    // ...bindings...;
    // end
    | Template of identifier * bindings: identifier list * arrDecl list * body: stmt
    | ConstantBind of identifier * rvalue // $ident <- rvalue  
    | Assignment of variable * expr
    | For of induc: identifier * lower: expr * upper: expr * body: stmt
    | While of cond: expr * body: stmt
    | IfElse of cond: expr * body: stmt
    | Suite of stmt list
    | Push of buffer: callback * lower: expr * upper: expr // push getBuffer() lower upper
    | Pop
    
and arrDecl =
    | Stack of isAST: bool * identifier * scalarType * int64
    | Heap of isAST: bool * identifier * scalarType   
    
// Notes:
// - Can only have decl once for a given variable in a syntax element, and it's always initialized at the top of the
// syntax component (either locally or in the ast)
// - All type stuff deferred to underlying language we generate for
// - scalars are int64, arrays you define, syntax elements are stack allocated
// - users are responsible for deleting heap allocated arrays
// - templates let you subsitute in things to write to. You can create new lvalues in them,
// except for AST lvalues. You can't access the local lvalues created in them though (you'd need an lvalue for that)
// - push and pop let you change the parsing stream like a stack. For push, you need to specify the new stream and lower/upper bounds
// pop will restore to the last version. This is helpful for when you need to parse out a separate stream (like in unstuffing for jpeg)
// I require you to give a name to transients because I think it makes it more clear what you are doing
// - Constant lets you bind a simple parsing rvalue to a name, and you can use that name as an identifier. Good for constants,
// like $EOI <- 0xFFD8 or whatever it is
// Templates, SyntaxElements, and ConstantBinds are all referenced with identifiers
// - you don't need to explicitly declare scalars because they will just have syntax elem/template scope. You need
// to define your arrays though
// - have compiler optionally warn users if have ast and local variable with the same name. You might want not want that!
// - local/ast variables are mutable, but they need the same general type for the rvalue (either a parsed scalar or the
// syntax element)
// - you can use the same transient variable name multiple times
// - libraries for TCAM?? that'd be fun