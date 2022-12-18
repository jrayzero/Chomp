module Chomp.parser

open FParsec
open AST

// Notes:
// |> => for combining parser generators (the normal forward pipe usage)
// a .>> b => parse a then b, return a's result
// a >>. b => parse a then b, return b's result
// a .>>. b => parse a then b, return both results in tuple

// Use to trace parsing (parser <!>)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

// Parse out innerParser between popen and pclose in stream
let betweenType popen pclose innerParser = (innerParser .>> spaces) |> between (skipString popen .>> spaces) (skipString pclose .>> spaces)

// Parse out a list of innerParser separated by delim between popen and pclose
let argLikeList popen pclose delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces) |> betweenType popen pclose
    
// like argLikeList, but without the open/close strings    
let freeArgLikeList delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces)
    
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

let singleIdentifierLevel() = 
    let isAsciiIdStart c = isAsciiLetter c || c = '_'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

let identifierRecord() =
    freeArgLikeList "." (singleIdentifierLevel()) |>> (fun i -> {levels=i})

let exprDU_Literal() =
    // notEmpty forces it to fail if it doesn't consume anything
    let hex = skipString "0x" >>. many1 hex .>> spaces 
    let decimal = many1 digit .>> spaces
    let binary = skipString "0b" >>. many1 (pstring "0" <|> pstring "1") .>> spaces
    let ascii = betweenType "\"" "\"" (many1 asciiLetter) .>> spaces
    (notEmpty (attempt hex |>> (fun l -> Literal(Hex, (l |> System.String.Concat).ToLower()))))
    <|> (notEmpty (attempt binary |>> (fun l -> Literal(Binary, l |> System.String.Concat))))
    <|> (notEmpty (attempt ascii |>> (fun l -> Literal(Ascii, l |> System.String.Concat))))
    <|> (decimal |>> (fun l -> Literal(Decimal, l |> System.String.Concat))) // this is the last line of defense!
        
//--------------------
// All the expression parser stuff is mostly copied from FParsec's calculator example
//--------------------
let opp = OperatorPrecedenceParser<expr,unit,unit>()

let exprDU_callback() =
    singleIdentifierLevel() .>>. (argLikeList "(" ")" "," (opp.ExpressionParser)) |>> fun (n,l) -> Callback({name=n;args=l})
    
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

opp.AddOperator(InfixOperator("||", spaces, 1, Associativity.Left, fun l r -> Or([l;r])))
opp.AddOperator(InfixOperator("&&", spaces, 2, Associativity.Left, fun l r -> And([l;r])))
opp.AddOperator(InfixOperator("|", spaces, 3, Associativity.Left, fun l r -> BOr([l;r])))
opp.AddOperator(InfixOperator("&", spaces, 4, Associativity.Left, fun l r -> BAnd([l;r])))
opp.AddOperator(InfixOperator("==", spaces, 5, Associativity.Left, fun l r -> Equals(true, [l;r])))
opp.AddOperator(InfixOperator("!=", spaces, 5, Associativity.Left, fun l r -> Equals(false, [l;r])))
opp.AddOperator(InfixOperator("<", spaces, 6, Associativity.Left, fun l r -> LessThan(false, l, r)))
opp.AddOperator(InfixOperator("<=", spaces, 6, Associativity.Left, fun l r -> LessThan(true, l, r)))
opp.AddOperator(InfixOperator(">", spaces, 6, Associativity.Left, fun l r -> GreaterThan(false, l, r)))
opp.AddOperator(InfixOperator(">=", spaces, 6, Associativity.Left, fun l r -> GreaterThan(false, l, r)))
opp.AddOperator(InfixOperator("<<", spaces, 7, Associativity.Left, fun l r -> LeftShift(l,r)))
opp.AddOperator(InfixOperator(">>", spaces, 7, Associativity.Left, fun l r -> RightShift(l,r)))
opp.AddOperator(InfixOperator("+", spaces, 8, Associativity.Left, fun l r -> Addition([l;r])))
opp.AddOperator(InfixOperator("-", spaces, 8, Associativity.Left, fun l r -> Subtraction([l;r])))
opp.AddOperator(InfixOperator("*", spaces, 9, Associativity.Left, fun l r -> Multiplication([l;r])))
opp.AddOperator(InfixOperator("/", spaces, 9, Associativity.Left, fun l r -> Division([l;r])))
opp.AddOperator(PrefixOperator("-", spaces, 10, true, Invert))
opp.AddOperator(PrefixOperator("~", spaces, 10, true, BInvert))
opp.AddOperator(PrefixOperator("!", spaces, 10, true, Not))

let completeExpr = spaces >>. opp.ExpressionParser

let program() =
    completeExpr .>> eof
    // exprDU() .>> eof
    
let myParser code =
    eprintfn "starting"
    let parsed = code |> run (program())
    parsed |> eprintfn "%A"