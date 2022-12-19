module Chomp.parser

open Chomp.AST
open FParsec

// Notes:
// |> => for combining parser generators (the normal forward pipe usage)
// a .>> b => parse a then b, return a's result
// a >>. b => parse a then b, return b's result
// a .>>. b => parse a then b, return both results in tuple

// TODO there are some egregious uses of backtracking, as well as spaces

let comment() = skipString "//" >>. skipRestOfLine true >>. many skipNewline

let commentSpaces() = spaces .>> opt(many (comment())) 

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
    
// like argLikeList, but without the open/close strings    
let freeArgLikeList delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces)
    
let freeArgLikeList1 delim innerParser =
    sepBy1 (innerParser .>> spaces) (skipString delim .>> spaces)    
    
let scalarDUT() =
    (skipString "int" >>.
        (skipString "8" |>> (fun _ -> Int8(true)))
        <|> (skipString "16" |>> (fun _ -> Int16(true)))
        <|> (skipString "32" |>> (fun _ -> Int32(true)))
        <|> (skipString "64" |>> (fun _ -> Int64(true))))
    <|> (skipString "uint" >>.
        (skipString "8" |>> (fun _ -> Int8(false)))
        <|> (skipString "16" |>> (fun _ -> Int16(false)))
        <|> (skipString "32" |>> (fun _ -> Int32(false)))
        <|> (skipString "64" |>> (fun _ -> Int64(false))))
    <|> (skipString "float" >>.
         (skipString "32" |>> (fun _ -> Float32))
         <|> (skipString "64" |>> (fun _ -> Float64)))

let singleIdentifierLevel() = 
    let isAsciiIdStart c = isAsciiLetter c || c = '_'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

let identifierRecord() =
    freeArgLikeList1 "." (singleIdentifierLevel()) |>> (fun i -> {levels=i})

let literalRecord() =
    let hex = skipString "0x" >>. many1 hex .>> commentSpaces() 
    let decimal = many1 digit .>> commentSpaces()
    let binary = skipString "0b" >>. many1 (pstring "0" <|> pstring "1") .>> commentSpaces()
    let ascii = betweenType "\"" "\"" (many1 asciiLetter) .>> commentSpaces()
    (notEmpty (attempt hex |>> (fun l -> {lt=Hex; value=(l |> System.String.Concat).ToLower()})))
    <|> (notEmpty (attempt binary |>> (fun l -> {lt=Binary; value=l |> System.String.Concat})))
    <|> (notEmpty (attempt ascii |>> (fun l -> {lt=Ascii; value=l |> System.String.Concat})))
    <|> (decimal |>> (fun l -> {lt=Decimal; value=l |> System.String.Concat})) // this is the last line of defense!
    
let exprDU_Literal() =
    literalRecord() |>> Literal
        
//--------------------
// All the expression parser stuff is mostly copied from FParsec's calculator example
//--------------------
let opp = OperatorPrecedenceParser<expr,unit,unit>()

let exprDU_callback() =
    identifierRecord() .>> spaces .>>. (argLikeList "(" ")" "," (opp.ExpressionParser)) |>> fun (n,l) -> Callback({name=n;args=l})
    
let exprDU_arrref() =
    identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> fun (n,l) -> ArrRef(variable.Default n, l)
    
let exprDU_variable() = identifierRecord() |>> fun v -> Variable(variable.Default v)    
    
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

let lvalueDU_ScalarL () =
    skipString "^" >>. identifierRecord() |>> fun i -> ScalarL(true, i)
    <|> (identifierRecord() |>> fun i -> ScalarL(false, i))
    
let lvalueDU_ArrL () =
    // skipString "^" >>. identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> fun (n,l) -> ArrL(true, n, l)
    identifierRecord() .>>. (betweenType "[" "]" opp.ExpressionParser) |>> fun (n,l) -> ArrL(n, l)
    
let lvalueDU() =
    attempt (notEmpty (lvalueDU_ArrL())) <|> lvalueDU_ScalarL()
    
let rvalueDU_ParseOnly() = betweenType "[" "]" (exprDU()) |>> ParseOnly
let rvalueDU_ParseAndValidate() =
    (betweenType "[" "]" (exprDU())) .>>. betweenType "{" "}" (freeArgLikeList1 "," (rangeDU())) |>> ParseAndValidate
    
let rvalueDU_Expr() = exprDU() |>> Expr

let rvalueDU() =
    attempt (notEmpty (rvalueDU_ParseAndValidate()))
    <|> rvalueDU_ParseOnly()
    <|> rvalueDU_Expr()

let ruleDU_PersistentLValue() =
    lvalueDU() .>> skipString ":=" .>> commentSpaces() .>>. rvalueDU() .>> skipString ";" .>> commentSpaces()  |>> PersistentLValue
    
let ruleDU_TransientLValue() =
    skipString "!" >>. identifierRecord() .>> skipString ":=" .>> commentSpaces() .>>. rvalueDU() .>> skipString ";" .>> commentSpaces() 
        |>> TransientLValue
        
let ruleDU_BindingLValue() =
    skipString "&" >>. (singleIdentifierLevel() .>> spaces |>> fun i -> {levels=[i]}) .>>
        skipString ":=" .>> commentSpaces() .>>. rvalueDU() .>> skipString ";" .>> commentSpaces()  |>> BindingLValue       
    
let ruleDU() =
    ruleDU_PersistentLValue() <|> ruleDU_TransientLValue() <|> ruleDU_BindingLValue()

type proxy = Proxy of identifier * int64

let arrDecls() =
    let heapAST = skipString "^" >>. spaces >>. scalarDUT()
               .>>. (skipString "[" .>> spaces .>> skipString "]" .>> spaces >>. singleIdentifierLevel() |>> fun i -> {levels=[i]})
                .>> spaces |>> fun (a,b) -> Heap(true, b, a)    
    let heapLocal = scalarDUT()
               .>>. (skipString "[" .>> spaces .>> skipString "]" .>> spaces >>. singleIdentifierLevel() |>> fun i -> {levels=[i]})
                .>> spaces |>> fun (a,b) -> Heap(false, b, a)
    let arraySz = skipString "[" .>> spaces >>. pint64 .>> spaces .>> (skipString "]")                 
    let stackAST = skipString "^" >>. spaces >>. scalarDUT()
                .>>. arraySz .>> spaces .>>. singleIdentifierLevel() .>> spaces 
                     |>> fun ((t,v),i) -> Stack(false, {levels=[i]}, t, v)
    let stackLocal = scalarDUT()
                .>>. arraySz .>> spaces .>>. singleIdentifierLevel() .>> spaces 
                     |>> fun ((t,v),i) -> Stack(false, {levels=[i]}, t, v)                      
    (attempt (heapAST <|> heapLocal) <|> (stackAST <|> stackLocal)) .>> spaces .>> skipString ";"  .>> commentSpaces()                    
    
let stmtParser,stmtRef = createParserForwardedToRef()

let stmtDU_Rule() = ruleDU() |>> Rule

let stmtDU_For() =
    let induction = skipString "for" >>. spaces1 >>. singleIdentifierLevel() |>> (fun i -> {levels=[i]}) .>> spaces
    let range = skipString "in" >>. spaces1 >>. exprDU() .>> spaces .>> skipString "to" .>> spaces1 .>>. exprDU() .>> spaces
    let body = betweenType "{" "}" (many stmtParser |>> Suite)
    let loop = induction .>>. range .>>. body .>> commentSpaces()
    loop |>> fun ((induc,(lower,upper)),body) -> For(induc,lower,upper,body)
    
let stmtDU_If() =
    let ifPart = skipString "if" >>. spaces1 >>. exprDU() .>>. betweenType "{" "}" (many stmtParser)
    let elsePart = opt (spaces >>. skipString "else" >>. commentSpaces() >>. betweenType "{" "}" (many stmtParser))
    let ifElse = ifPart .>>. elsePart .>> commentSpaces()
    ifElse |>> fun ((cond,iBody),eBody) ->
        match eBody with
            | Some(l) -> IfElse(cond, Suite(iBody), Some(Suite(l)))
            | None -> IfElse(cond, Suite(iBody), None)

let markerDU_LiteralMarker() =
    literalRecord() |>> LiteralMarker
    
let markerDU_ConstantMarker() =
    identifierRecord() |>> fun i -> ConstantMarker(variable.Default i)
    
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
        skipString "syntax" >>. spaces1 >>. singleIdentifierLevel() .>> commentSpaces() |>> fun i -> {levels=[i]}
    let stmts = spaces >>. (many (stmtDU()))
    let arrs = spaces >>. opt (skipString "arrDecls" >>. commentSpaces() >>. betweenType "{" "}" (many (arrDecls())))
    let body = betweenType "{" "}" (arrs .>>. stmts)
    decl .>>. body |>> fun (id,(arrs,body)) ->
        match arrs with
            | Some(a) -> Syntax(id,a,Suite(body))
            | None -> Syntax(id,[],Suite(body))
    
let elementDU_Template() =
    let decl =
        skipString "template" >>. spaces1 >>. (singleIdentifierLevel() .>> spaces |>> fun i -> {levels=[i]}) .>>. argLikeList "(" ")" "," (identifierRecord())
    let stmts = spaces >>. (many (stmtDU()))
    let arrs = spaces >>. opt (skipString "arrDecls" >>. commentSpaces() >>. betweenType "{" "}" (many (arrDecls())))
    let body = betweenType "{" "}" (arrs .>>. stmts)
    decl .>>. body |>> fun ((id,bindings),(arrs,body)) ->
        match arrs with
            | Some(a) -> Template(id,bindings,a,Suite(body))
            | None -> Template(id,bindings,[],Suite(body))
    
let elementDU_Constant() =
    skipString "constant" >>. spaces1 >>. (singleIdentifierLevel() |>> fun i -> {levels=[i]})
        .>>. (spaces >>. skipString ":=" >>. commentSpaces() >>. literalRecord() .>> commentSpaces() .>> skipString ";" .>> commentSpaces()) |>> Constant
    
let elementDU() =
    elementDU_Template() <|> elementDU_Syntax() <|> elementDU_Constant()
    
let programDU() =
    commentSpaces() >>. many (elementDU()) |>> Program
    
let parseIt code =
    eprintfn "starting"
    let parsed = code |> run (programDU() .>> eof)
    parsed |> eprintfn "%A"