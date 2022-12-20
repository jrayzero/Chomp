module Chomp.parser

open Chomp.AST
open FParsec

// Notes:
// |> => for combining parser generators (the normal forward pipe usage)
// a .>> b => parse a then b, return a's result
// a >>. b => parse a then b, return b's result
// a .>>. b => parse a then b, return both results in tuple

// TODO there are some egregious uses of backtracking, as well as spaces

let comment() = attempt (spaces .>> skipString "//" >>. skipRestOfLine true >>. many skipNewline)

let commentSpaces() = spaces .>> many (comment()) 

// Use to trace parsing (parser <!>)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

// Parse out innerParser between popen and pclose in stream
let betweenType popen pclose innerParser = (innerParser .>> spaces) |> between (skipString popen .>> commentSpaces()) (skipString pclose .>> commentSpaces())

// Parse out a list of innerParser separated by delim between popen and pclose
let argLikeList popen pclose delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces) |> betweenType popen pclose
    
let argLikeList1 popen pclose delim innerParser =
    sepBy1 (innerParser .>> spaces) (skipString delim .>> spaces) |> betweenType popen pclose    
    
// like argLikeList, but without the open/close strings    
let freeArgLikeList delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces)
    
let freeArgLikeList1 delim innerParser =
    sepBy1 (innerParser .>> spaces) (skipString delim .>> spaces)    

let singleIdentifierLevel() = 
    let isAsciiIdStart c = isAsciiLetter c || c = '_'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

let identifierRecord() =
    freeArgLikeList1 "." (singleIdentifierLevel()) |>> (fun i -> {levels=i})

let scalarTypeDU() =
    (skipString "int" >>.
        ((skipString "8" |>> (fun _ -> Int8(true)))
        <|> (skipString "16" |>> (fun _ -> Int16(true)))
        <|> (skipString "32" |>> (fun _ -> Int32(true)))
        <|> (skipString "64" |>> (fun _ -> Int64(true)))))
    <|> (skipString "uint" >>.
        ((skipString "8" |>> (fun _ -> Int8(false)))
        <|> (skipString "16" |>> (fun _ -> Int16(false)))
        <|> (skipString "32" |>> (fun _ -> Int32(false)))
        <|> (skipString "64" |>> (fun _ -> Int64(false)))))
    <|> (skipString "float" >>.
         ((skipString "32" |>> (fun _ -> Float32))
         <|> (skipString "64" |>> (fun _ -> Float64))))
    <|> (singleIdentifierLevel() |>> SyntaxRef)  

let literalRecord() =
    let hex = skipString "0x" >>. many1 hex .>> commentSpaces() 
    let decimal = many1 digit .>> commentSpaces()
    let binary = skipString "0b" >>. many1 (pstring "0" <|> pstring "1") .>> commentSpaces()
    let ascii = betweenType "\"" "\"" (many1 asciiLetter) .>> commentSpaces()
    (notEmpty (attempt hex |>> (fun l -> {lit=Hex; value=(l |> System.String.Concat).ToLower()})))
    <|> (notEmpty (attempt binary |>> (fun l -> {lit=Binary; value=l |> System.String.Concat})))
    <|> (notEmpty (attempt ascii |>> (fun l -> {lit=Ascii; value=l |> System.String.Concat})))
    <|> (decimal |>> (fun l -> {lit=Decimal; value=l |> System.String.Concat})) // this is the last line of defense!
    
let exprDU_Literal() =
    literalRecord() |>> Literal
        
//--------------------
// All the expression parser stuff is mostly copied from FParsec's calculator example
//--------------------
let opp = OperatorPrecedenceParser<expr,unit,unit>()

let exprDU_callback() =
    singleIdentifierLevel() .>> spaces .>>. (argLikeList "(" ")" "," opp.ExpressionParser)
    |>> fun (n,l) -> Callback({name=n;args=l})
    
let exprDU_arrref() =
    identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> fun (n,l) -> ArrRef({name=n}, l)
    
let exprDU_variable() = identifierRecord() |>> fun v -> Variable({name=v})    
    
// literal, variable, callback, arref
let exprDU_Primitives() =
    // attempts for these first two since they can contain vars
    let callback = attempt (notEmpty (exprDU_callback()))
    let arrref = attempt (notEmpty (exprDU_arrref()))
    let var = notEmpty (exprDU_variable()) 
    callback <|> arrref <|> var <|> exprDU_Literal()

opp.TermParser <- exprDU_Primitives() <|> betweenType "(" ")" opp.ExpressionParser

opp.AddOperator(InfixOperator("||", commentSpaces(), 1, Associativity.Left, fun l r -> Or([l;r])))
opp.AddOperator(InfixOperator("&&", commentSpaces(), 2, Associativity.Left, fun l r -> And([l;r])))
opp.AddOperator(InfixOperator("|", commentSpaces(), 3, Associativity.Left, fun l r -> BOr([l;r])))
opp.AddOperator(InfixOperator("&", commentSpaces(), 4, Associativity.Left, fun l r -> BAnd([l;r])))
opp.AddOperator(InfixOperator("==", commentSpaces(), 5, Associativity.Left, fun l r -> Equals(true, [l;r])))
opp.AddOperator(InfixOperator("!=", commentSpaces(), 5, Associativity.Left, fun l r -> Equals(false, [l;r])))
opp.AddOperator(InfixOperator("<", commentSpaces(), 6, Associativity.Left, fun l r -> LessThan(false, l, r)))
opp.AddOperator(InfixOperator("<=", commentSpaces(), 6, Associativity.Left, fun l r -> LessThan(true, l, r)))
opp.AddOperator(InfixOperator(">", commentSpaces(), 6, Associativity.Left, fun l r -> GreaterThan(false, l, r)))
opp.AddOperator(InfixOperator(">=", commentSpaces(), 6, Associativity.Left, fun l r -> GreaterThan(false, l, r)))
opp.AddOperator(InfixOperator("<<", commentSpaces(), 7, Associativity.Left, fun l r -> LeftShift(l,r)))
opp.AddOperator(InfixOperator(">>", commentSpaces(), 7, Associativity.Left, fun l r -> RightShift(l,r)))
opp.AddOperator(InfixOperator("+", commentSpaces(), 8, Associativity.Left, fun l r -> Addition([l;r])))
opp.AddOperator(InfixOperator("-", commentSpaces(), 8, Associativity.Left, fun l r -> Subtraction([l;r])))
opp.AddOperator(InfixOperator("*", commentSpaces(), 9, Associativity.Left, fun l r -> Multiplication([l;r])))
opp.AddOperator(InfixOperator("/", commentSpaces(), 9, Associativity.Left, fun l r -> Division([l;r])))
opp.AddOperator(PrefixOperator("-", commentSpaces(), 10, true, Invert))
opp.AddOperator(PrefixOperator("~", commentSpaces(), 10, true, BInvert))
opp.AddOperator(PrefixOperator("!", commentSpaces(), 10, true, Not))

let exprDU() = commentSpaces() >>. opp.ExpressionParser

let rangeDU_Single() = pint64 |>> Single
let rangeDU_Lower() = pint64 .>> skipString ".." .>> spaces |>> Lower
let rangeDU_Upper() = skipString ".." >>. spaces >>. pint64 |>> Upper
let rangeDU_Range() = pint64 .>>. (skipString ".." >>. spaces >>. pint64) |>> Range
let rangeDU () = 
    attempt (notEmpty (rangeDU_Range()))
    <|> attempt (notEmpty (rangeDU_Upper()))
    <|> attempt (notEmpty (rangeDU_Lower()))
    <|> rangeDU_Single()

let scalarDeclarationExpr() =
    singleIdentifierLevel() .>> spaces .>> skipString "::" .>> spaces .>>. scalarTypeDU() .>> spaces
    
let scalarDeclarationRecord() =
    singleIdentifierLevel() .>> spaces .>> skipString "::" .>> spaces .>>. scalarTypeDU() .>> commentSpaces()
        .>> skipString ";" .>> commentSpaces() |>> fun (x,y) -> {name=x; t=y} 

let arrayHeapDeclarationExpr() =
    singleIdentifierLevel() .>> spaces .>> skipString "::" .>> spaces .>>. scalarTypeDU() .>> spaces .>> skipString "[]" .>> spaces

let arrayDeclarationDU() =
    let heap = singleIdentifierLevel() .>> spaces .>> skipString "::" .>>. scalarTypeDU() .>> spaces
               .>> skipString "[]" .>> commentSpaces() .>> skipString ";" .>> commentSpaces() |>> Heap
    let stack = singleIdentifierLevel() .>> spaces .>> skipString "::" .>>. scalarTypeDU() .>> spaces
               .>> skipString "[" .>>. pint64 .>> skipString "]" .>> commentSpaces() .>> skipString ";"
                .>> commentSpaces() |>> fun ((a,b),c) -> Stack(a,b,c)
    attempt heap <|> stack                

let anyDeclarationDU() =
    (attempt (arrayDeclarationDU() |>> ArrayDeclaration))
     <|> (scalarDeclarationRecord() |>> anyDeclaration.ScalarDeclaration)
     
let lhsDU() =
    identifierRecord() .>>. opt (skipString "[" >>. spaces >>. exprDU() .>> spaces .>> skipString "]")
        .>> commentSpaces() |>> fun (x,y) ->
            match y with
                | Some(e) -> ArrayLhs(x,e)
                | None -> ScalarLhs(x)
    
// let lvalueDU_ArrL () =
//     // skipString "^" >>. identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> fun (n,l) -> ArrL(true, n, l)
//     identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> ArrayLhs
//     
// let lvalueDU() =
//     attempt (notEmpty (lvalueDU_ArrL())) <|> lvalueDU_ScalarL()
    
let rhsDU_ParseBits() = betweenType "[" "]" (exprDU()) |>> ParseBits
let rhsDU_ParseBitsAndValidate() =
    (betweenType "[" "]" (exprDU())) .>>. (argLikeList1 "{" "}" "," (rangeDU())) |>> ParseBitsAndValidate  
let rhsDU_ParseElement() =
    (betweenType "<" ">" (singleIdentifierLevel() |>> ParseElement))
let rhsDU() =
    attempt (rhsDU_ParseBitsAndValidate())
    <|> rhsDU_ParseBits()
    <|> rhsDU_ParseElement()
    <|> (exprDU() |>> Expr)
    
// let rvalueDU_Expr() = exprDU() |>> Expr

// let rvalueDU() =
    // ((parsingRValueDU() |>> ParsingRValue) <|> rvalueDU_Expr()) .>> commentSpaces()

// let ruleDU_PersistentLValue() =
    // lvalueDU() .>> skipString ":=" .>> commentSpaces() .>>. rvalueDU() .>> skipString ";" .>> commentSpaces()  |>> PersistentLValue
    
// let ruleDU_TransientLValue() =
    // skipString "$" >>. spaces >>. skipString ":=" >>. commentSpaces() >>. parsingRValueDU() .>> skipString ";" .>> commentSpaces() 
        // |>> TransientLValue

let ruleDU_ScalarDeclaration() = scalarDeclarationRecord() |>> rule.RuleScalarDeclaration
let ruleDU_Assignment() = lhsDU() .>> spaces .>> skipString ":=" .>> commentSpaces() .>>. rhsDU() .>> commentSpaces()
                          .>> skipString ";" .>> commentSpaces() |>> Assignment
let ruleDU_Transient() = rhsDU() .>> commentSpaces() .>> skipString ";" .>> commentSpaces() |>> Transient                          
        
let ruleDU() =
    attempt (notEmpty (ruleDU_ScalarDeclaration()))
    <|> (attempt (notEmpty (ruleDU_Assignment())))
    <|> ruleDU_Transient()

// let arrDecls() =
//     let heapAST = skipString "^" >>. spaces >>. scalarDUT()
//                .>>. (skipString "[" .>> spaces .>> skipString "]" .>> spaces >>. singleIdentifierLevel())
//                 .>> spaces |>> fun (a,b) -> Heap(true, b, a)    
//     let heapLocal = scalarDUT()
//                .>>. (skipString "[" .>> spaces .>> skipString "]" .>> spaces >>. singleIdentifierLevel())
//                 .>> spaces |>> fun (a,b) -> Heap(false, b, a)
//     let arraySz = skipString "[" .>> spaces >>. pint64 .>> spaces .>> (skipString "]")                 
//     let stackAST = skipString "^" >>. spaces >>. scalarDUT()
//                 .>>. arraySz .>> spaces .>>. singleIdentifierLevel() .>> spaces 
//                      |>> fun ((t,v),i) -> Stack(false, i, t, v)
//     let stackLocal = scalarDUT()
//                 .>>. arraySz .>> spaces .>>. singleIdentifierLevel() .>> spaces 
//                      |>> fun ((t,v),i) -> Stack(false, i, t, v)                      
//     (attempt (heapAST <|> heapLocal) <|> (stackAST <|> stackLocal)) .>> spaces .>> skipString ";"  .>> commentSpaces()                    
    
let stmtParser,stmtRef = createParserForwardedToRef()

let stmtDU_Rule() = ruleDU() |>> Rule

let stmtDU_For() =
    let induction = skipString "for" >>. spaces1 >>. singleIdentifierLevel() |>> (fun i -> {levels=[i]}) .>> spaces
    let range = skipString "in" >>. spaces1 >>. exprDU() .>> spaces .>> skipString "to" .>> spaces1 .>>. exprDU()
    let body = betweenType "{" "}" (many (commentSpaces() >>. stmtParser) |>> Suite)
    let loop = induction .>>. range .>>. body .>> commentSpaces()
    loop |>> fun ((induc,(lower,upper)),body) -> For(induc,lower,upper,body)
    
let stmtDU_If() =
    let ifPart = skipString "if" >>. spaces1 >>. exprDU() .>>. betweenType "{" "}" (many (commentSpaces() >>. stmtParser))
    let elsePart = opt (spaces >>. skipString "else" >>. commentSpaces() >>. betweenType "{" "}" (many (commentSpaces() >>. stmtParser)))
    let ifElse = ifPart .>>. elsePart .>> commentSpaces()
    ifElse |>> fun ((cond,iBody),eBody) ->
        match eBody with
            | Some(l) -> IfElse(cond, Suite(iBody), Some(Suite(l)))
            | None -> IfElse(cond, Suite(iBody), None)

let markerDU_LiteralMarker() =
    literalRecord() |>> LiteralMarker
    
let markerDU_ConstantMarker() =
    singleIdentifierLevel() |>> ConstantMarker
    
let markerDU() =
    markerDU_LiteralMarker() <|> markerDU_ConstantMarker()

let markerBody() = skipString "marker" >>. spaces1 >>. markerDU() .>> spaces .>>. betweenType "{" "}" ((many stmtParser) |>> Suite)

let stmtDU_Alternate() =
    skipString "alternate" >>. spaces1 >>. betweenType "{" "}" (many1 (markerBody())) |>> Alternate
            
let stmtDU_Push() =
    skipString "push" >>. spaces1 >>. exprDU() .>>. exprDU() .>>. exprDU() .>> spaces .>> skipString ";" .>> commentSpaces() 
        |>> fun ((buff,lower),upper) -> Push(buff,lower,upper)

let stmtDU_Pop() = skipString "pop" .>> spaces .>> skipString ";" .>> commentSpaces()  |>> fun _ -> Pop

let stmtDU() =
    let p = stmtDU_For() <|> stmtDU_If() <|> stmtDU_Alternate() <|> stmtDU_Push() <|> stmtDU_Pop() <|> stmtDU_Rule()
    stmtRef := p
    p
    
let elementDU_Syntax() =
    let decl =
        skipString "syntax" >>. spaces1 >>. singleIdentifierLevel() .>> commentSpaces() 
    let ast = spaces >>. opt (skipString "ast" >>. commentSpaces() >>. betweenType "{" "}" (many (anyDeclarationDU())))
    let local = spaces >>. opt (skipString "local" >>. commentSpaces()
                                >>. betweenType "{" "}" (many (arrayDeclarationDU())))
    let stmts = commentSpaces() >>. (many (commentSpaces() >>. stmtDU()))        
    let body = betweenType "{" "}" (ast .>>. local .>>. stmts)
    decl .>>. body |>> fun (name,((ast,local),body)) ->
        match ast with
            | Some(a) ->
                match local with
                    | Some(l) -> Syntax(name, a, l, Suite(body))
                    | None -> Syntax(name, a, [], Suite(body))
            | None ->
                match local with
                    | Some(l) -> Syntax(name, [], l, Suite(body))
                    | None -> Syntax(name, [], [], Suite(body))

let templateBinding() =
    let array = arrayHeapDeclarationExpr() |>> fun (x,y) -> (true,x,y)
    let scalar = scalarDeclarationExpr() |>> fun (x,y) -> (false,x,y)
    
    let refVar = skipString "&" >>. ((attempt array) <|> scalar) |>> fun (a,b,c) ->
                    if a then ArrayBinding(true,c,b) else ScalarBinding(true,c,b)
    let nonRefVar = ((attempt array) <|> scalar) |>> fun (a,b,c) ->
                    if a then ArrayBinding(false,c,b) else ScalarBinding(false,c,b)                    
    refVar <|> nonRefVar
    
let elementDU_Template() =
    let decl =
        skipString "template" >>. spaces1 >>. singleIdentifierLevel() .>> spaces
        .>>. argLikeList "(" ")" "," (templateBinding()) .>> commentSpaces() 
    let local = spaces >>. opt (skipString "local" >>. commentSpaces()
                                >>. betweenType "{" "}" (many (arrayDeclarationDU())))
    let stmts = commentSpaces() >>. (many (commentSpaces() >>. stmtDU()))            
    let body = betweenType "{" "}" (local .>>. stmts)
    decl .>>. body |>> fun ((name,bindings),(local,body)) ->
        match local with
            | Some(l) -> Template(name, bindings, l, Suite(body))
            | None -> Template(name, bindings, [], Suite(body))
    
let elementDU_Constant() =
    skipString "constant" >>. spaces1 >>. singleIdentifierLevel()
        .>>. (spaces >>. skipString ":=" >>. commentSpaces() >>. literalRecord() .>> commentSpaces()
              .>> skipString ";" .>> commentSpaces()) |>> Constant
    
let elementDU() =
    elementDU_Template() <|> elementDU_Syntax() <|> elementDU_Constant()
    
let programDU() =
    commentSpaces() >>. many (elementDU()) |>> Program
    
let parseIt code =
    eprintfn "starting"
    let parsed = code |> run (programDU() .>> eof)
    parsed |> eprintfn "%A"
    match parsed with
        | Success(a,_,_) -> a
        | Failure _ -> failwith "Parse of input failed!"