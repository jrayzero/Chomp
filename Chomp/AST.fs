module Chomp.AST

// Scoping:
// All variables are element-scoped (i.e. within Template or Syntax). This emans we can pre-gather all the variables.
// If you use a variable somewhere that's not an lvalue, it's first checked against the element-scoped variables
// (arrays, scalars, and nested syntax)
// If it's not found, then the constants are checked. If it's still not found, then it's invalid


// bits are parsed from msb to lsb, which is the most common
// we handle the bit packing for you

// built-ins

// TODO use unique pointers for holding the nested structures

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
    | Literal of literal
    
and literal = { lit : literalType; value: string}    
    
and callback = { name: identifier; args: expr list }

and variable =
    { user: identifier; fqn: identifier }
    static member Default user =
        { user = user; fqn = user }

// and variableKind =
    // | SyntaxRef
    // | TemplateRef
    // | LocalRef
    // | ASTRef
    // | TemplateBinding
    // | Constant
    // | Undef // not know yet

// used for ParseAndVAlidate    
type range =
    | Single of int64 // value
    | Lower of int64 // lower..
    | Upper of int64 // ..upper
    | Range of lower: int64 * upper: int64 // lower..upper

// TODO I want ^ to be @, but that seems to cause issues with just reading in the actual dotnet command line (@ might be special?)
// these are decls, but are assignments
type rule =
    // transients don't have general lvalue since it doesn't make sense for an array to be transient
    // the array doesn't need persistent b/c that is handled in the arrdecls range
    // you need it for scalars since we don't pre-declare those
    | PersistentLValue of lvalue * rvalue // ^x/x/x[idx] := rvalue;
    // Lets you just skip over it. Require a name though to make it clearer what you're parsing
    | TransientLValue of identifier * rvalue // !x := rvalue;
    | BindingLValue of identifier * rvalue // &x := rvalue; only for use in templates--references the binding list
    
and lvalue =
    | ScalarL of isAST: bool * identifier // x or ^x
    | ArrL of identifier * expr // x[idx]
    
// determines the type of the lvalue     
and rvalue =
    | ParseOnly of expr // [20] or [SyntaxGroup/template/constant] if lvalue is a storage, keep the value, otherwise just toss it
    | ParseAndValidate of expr * rhs: range list // [20]{0..2,10} parse the value and then compare to the rhs possibilities
    | Expr of expr // a + b, lets you just assign to some arbitrary expr
    
type element =
    // syntax ident {
    //   arrDecls { }
    //   ...rules...
    // }
    | Syntax of identifier * arrDecl list * body: stmt
    // template ident(bindings...) {
    //   arrDecls { ... }
    //   ...rules...
    // }
    // cannot contain AST rules
    | Template of identifier * bindings: identifier list * arrDecl list * body: stmt
    // constant SOS := <literal>;
    | Constant of identifier * literal
    
and stmt =
    | Rule of rule
    // for i in lower to upper { body }
    | For of induc: identifier * lower: expr * upper: expr * body: stmt
    // if cond { } [else { }]
    | IfElse of cond: expr * tBody: stmt * fBody: option<stmt>
    // alternate { marker <marker> { } marker <marker> { } }
    | Alternate of (marker * stmt) list
    // just a list of stmts. not parsed individually
    | Suite of stmt list
    // push getBuffer() lower upper;
    | Push of buffer: expr * lower: expr * upper: expr
    // pop;
    | Pop
    
// backtracks    
and marker =
    | LiteralMarker of literal
    | ConstantMarker of variable // compiler checks that it is a constant!
    
and arrDecl =
    | Stack of isAST: bool * identifier * scalarType * int64
    | Heap of isAST: bool * identifier * scalarType
    
type program = Program of element list    
    
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