module Chomp.AST

// Scoping:
// All variables are element-scoped (i.e. within Template or Syntax). This emans we can pre-gather all the variables.
// If you use a variable somewhere that's not an lvalue, it's first checked against the element-scoped variables
// (arrays, scalars, and nested syntax)
// If it's not found, then the constants are checked. If it's still not found, then it's invalid


// bits are parsed from msb to lsb, which is the most common
// we handle the bit packing for you

// TODO use unique pointers for holding the nested structures

type scalarType =
    | Int8 of signed:bool
    | Int16 of signed:bool
    | Int32 of signed:bool
    | Int64 of signed:bool
    | Bool
    | Float32
    | Float64
    | SyntaxRef of string // a reference to a syntax class

type identifier = { levels: string list }

// either for expressions (where it is a number) or for parsing
type literalType =
    | Hex
    | Decimal
    | Binary
    | Ascii
    
type variable = { name: identifier; }

type literal = { lit : literalType; value: string}   
    
// the numbers represent the precedence (copies from C)
// we only have left-associativity, so we don't need to worry really
// about handling things specially at the same precedence level
type expr =
    // 0
    | Callback of callback // foo(args...)
    | ArrRef of identifier * idx: expr // arr[idx]
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
    
and callback = { name: string; args: expr list }

type range =
    | Single of int64 // value
    | Lower of int64 // lower..
    | Upper of int64 // ..upper
    | Range of lower: int64 * upper: int64 // lower..upper
    
type scalarDeclaration = { name: string; t: scalarType } // x::<type>;

type arrayDeclaration =
    | Stack of string * scalarType * int64 // x::<type>[<sz>];
    | Heap of string * scalarType * expr// @x::<type>[<sz>];
    
type anyDeclaration =
    | ScalarDeclaration of scalarDeclaration // x::<type>;
    | ArrayDeclaration of arrayDeclaration //  x::<type>[<sz>];

type lhs =
    | ScalarLhs of identifier // a.b.c 
    | ArrayLhs of identifier * expr // a.b.c[idx]
    
type rhs =
    | ParseBits of expr // [20] if lvalue is a storage, keep the value, otherwise just toss it
    | ParseBitsAndValidate of expr * rhs: range list // [20]{0..2,10} parse the value and then compare to the rhs possibilities
    | ParseElement of string // <SyntaxGroup/constant> (can't actually have template when you are assignment, but can have it for transient)
    | ParseLiteral of literal // <literal>
    | ParseTemplate of string * expr list // <template(bindings...)>
    | Expr of expr // a + b, lets you just assign to some arbitrary expr. doesn't parse anything

type rule =
    | ScalarDeclarationAssign of scalarDeclaration * option<rhs> // x::int8; x::syntaxType;
    | ArrayDeclarationOnly of arrayDeclaration
    | Assignment of lhs * rhs // x[idx] := rvalue; or x := rvalue;
    | Transient of rhs // rvalue;
    
type binding =
    | ArrayBinding of ref:bool * scalarType * string // arr::int8[] or &arr::int8[] (no diff btw heap/stack)
    | ScalarBinding of ref:bool * scalarType * string // val::int8 or &val::int8
    
type element =
    // syntax ident {
    //   ast {
    //     arrAst::int8[];
    //     val::someSyntaxElem;
    //     val2::float;
    //   }
    //   ...stmts...
    // }
    | Syntax of string * ast: anyDeclaration list * body: stmt
    // template ident(bindings...) {
    //   ...stmts...
    // }
    | Template of string * bindings: binding list * body: stmt
    // constant SOS := <literal>;
    | Constant of string * literal

and stmt =
    | Rule of rule
    // for i in lower to upper { body }
    | For of induc: string * lower: expr * upper: expr * body: stmt
    // if cond { } [else { }]
    | IfElse of cond: expr * tBody: stmt * fBody: option<stmt>
    // alternate { marker <marker> { } marker <marker> { } }
    | Alternate of (marker * stmt) list
    // just a list of stmts. not parsed individually
    | Suite of stmt list
    // just an expression (used internally--this would be parsed as a rule in the frontend)
    | ExprStmt of expr
    // push getBuffer() lower upper;
    | Push of buffer: expr * lower: expr * upper: expr
    // pop;
    | Pop
    
// backtracks    
and marker =
    | LiteralMarker of literal
    | ConstantMarker of string
    
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