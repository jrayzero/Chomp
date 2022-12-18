module Chomp.parser

open FParsec
open AST

// TODO this uses pretty egregious backtracking in some places, but I just wanted to get it working. Clean it up at some point.

// Notes:
// |> => for combining parser generators (the normal forward pipe usage)
// a .>> b => parse a then b, return a's result
// a >>. b => parse a then b, return b's result
// a .>>. b => parse a then b, return both results in tuple

// Parse out innerParser between popen and pclose in stream
let betweenType popen pclose innerParser = (innerParser .>> spaces) |> between (skipString popen .>> spaces) (skipString pclose .>> spaces)

// Parse out a list of innerParser separated by delim between popen and pclose
let argLikeList popen pclose delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces) |> betweenType popen pclose
    
// like argLikeList, but without the open/close strings    
let freeArgLikeList delim innerParser =
    sepBy (innerParser .>> spaces) (skipString delim .>> spaces)

let exprParser, exprRef = createParserForwardedToRef<expr,unit>()
    
let scalarDUT() =
    // TODO is attempt the correct  hing here??
    (attempt (skipString "int8") |>> (fun _ -> Int8(true)))
    <|> (attempt (skipString "int16") |>> (fun _ -> Int16(true)))
    <|> (attempt (skipString "int32") |>> (fun _ -> Int32(true)))
    <|> (attempt (skipString "int64") |>> (fun _ -> Int64(true)))
    <|> (attempt (skipString "uint8") |>> (fun _ -> Int8(false)))
    <|> (attempt (skipString "uint16") |>> (fun _ -> Int16(false)))
    <|> (attempt (skipString "uint32") |>> (fun _ -> Int32(false)))
    <|> (attempt (skipString "uint64") |>> (fun _ -> Int64(false)))
    <|> (attempt (skipString "float32") |>> (fun _ -> Float32))
    <|> (attempt (skipString "float64") |>> (fun _ -> Float64))

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

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let exprDU_callback() =
    singleIdentifierLevel() .>>. (argLikeList "(" ")" "," (exprParser)) |>> fun (n,l) -> Callback({name=n;args=l})
    
let exprDU_arrref() =
    identifierRecord() .>>. (betweenType "[" "]" exprParser) |>> fun (n,l) -> ArrRef(variable.Default n, l)
    
let exprDU_variable() = identifierRecord() |>> fun v -> Variable(variable.Default v)    
    
// literal, variable, callback, arref
let exprDU_Primitives() =
    // attempts for these first two since they can contain vars
    let callback = attempt (notEmpty (exprDU_callback()))
    let arrref = attempt (notEmpty (exprDU_arrref()))
    let var = notEmpty (exprDU_variable()) 
    callback <|> arrref <|> var <|> exprDU_Literal()
    
let exprDU_LastLevel() = notEmpty (betweenType "(" ")" exprParser) <|> exprDU_Primitives()

// true binary
let binary op next =
    let lhs = next() .>> spaces
    let op = skipString op .>> spaces
    let rhs = opt (op >>. next() .>> spaces)
    lhs .>>. rhs
    
let manyBinary op next =
    let lhs = next() .>> spaces
    let op = skipString op .>> spaces
    let rhs = many (op >>. next() .>> spaces)
    lhs .>>. rhs    

let binaryOp op t next =
    manyBinary op next |>>
        fun (l,r) ->
            if r.Length > 0 then
                t(l::r)
            else
                l
                
// like binaryOp, but doesn't produce a list for the operands
// uses curried function for t                
let binaryCurryLR op t next =
    binary op next |>>
        fun (l,r) -> 
            match r with
                | Some(e) -> t l e
                | None -> l

// like binaryLR but just takes a DU for t                
let binaryLR op t next =
    binary op next |>>
        fun (l,r) ->
            match r with
                | Some(e) -> t(l, e)
                | None -> l                     
    
let unary op t next =
    let lhs = opt (skipString op >>. spaces) .>>. next() |>>
              fun (o,e) ->
                  match o with
                    | Some _ -> t e
                    | None -> e
    lhs                    
    
let exprDU_Or() = binaryOp "||" Or exprDU_LastLevel
let exprDU_And() = binaryOp "&&" And exprDU_Or
let exprDU_BOr() = binaryOp "|" BOr exprDU_And
let exprDU_BAnd() = binaryOp "&" BAnd exprDU_BOr
let exprDU_Eq() = binaryOp "==" (EqCurry true) exprDU_BAnd
let exprDU_NotEq() = binaryOp "!=" (EqCurry false) exprDU_Eq
let exprDU_LTE() = binaryCurryLR "<=" (LTCurry true) exprDU_NotEq
let exprDU_LT() = binaryCurryLR "<" (LTCurry false) exprDU_LTE
let exprDU_GTE() = binaryCurryLR ">=" (GTCurry true) exprDU_LT
let exprDU_GT() = binaryCurryLR ">" (GTCurry false) exprDU_GTE
let exprDU_RShift() = binaryLR ">>" LeftShift exprDU_GT
let exprDU_LShift() = binaryLR "<<" LeftShift exprDU_RShift
let exprDU_Subtraction() = binaryOp "-" Subtraction exprDU_LShift
let exprDU_Addition() = binaryOp "+" Addition exprDU_Subtraction
let exprDU_Division() = binaryOp "/" Division exprDU_Addition
let exprDU_Multiplication() = binaryOp "*" Multiplication exprDU_Division
let exprDU_Not() = unary "!" Not exprDU_Multiplication
let exprDU_BInvert() = unary "~" BInvert exprDU_Not
let exprDU_Invert() = unary "-" Invert exprDU_BInvert
let exprDU() =
    exprDU_Invert()
    
exprRef.Value <- exprDU()    

let program() =
    // scalarType()
    // identifierType() 
    // exprDU_Literal() .>> eof
    
    // let lhs = opt (skipString "-" >>. spaces) >>. pint64 |>> fun y -> Invert(Literal(Decimal, sprintf "%d" y))
    
    // exprDU_callback() .>> eof
    exprDU() .>> eof
    // lhs .>> eof
    
let myParser code =
    eprintfn "starting"
    let parsed = code |> run (program())
    parsed |> eprintfn "%A"