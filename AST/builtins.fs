module AST.builtins

open AST

let bufferName = "buffer"
let bufferVar = {name={levels=[bufferName]}}

let cursorName = "cursor"
let cursorVar = {name={levels=[cursorName]}}

let stopName = "stop"
let stopVar = {name={levels=[stopName]}}

let userName = "user"
let userVar = {name={levels=[userName]}}

let syntaxParseName = "syntaxParse"
let syntaxParse name = Callback({name=syntaxParseName; args=[Literal({lit=Ascii; value=name})
                                                             Variable(userVar)
                                                             Variable(bufferVar)
                                                             Variable(cursorVar)]})

let templateParseName = "templateParse"
let templateParse name bindings =
    let args = Literal({lit=Ascii; value=name}) :: Variable(userVar)
                :: Variable(bufferVar) :: Variable(cursorVar) :: bindings
    Callback({name=templateParseName; args=args})

let exists nbits = Callback({name="exists"; args=[Variable(cursorVar);Variable(stopVar);nbits]})

let fatal msg = Callback({name="fatal"; args=[Literal({lit=Ascii; value=msg})]})

// placeholder for a parsing routine on the buffer
let parseBitsName = "parseBits"
let parseBits nbits typeName = Callback({name=parseBitsName; args=[Literal({lit=Ascii; value=typeName})
                                                                   Variable(bufferVar)
                                                                   Variable(cursorVar)
                                                                   nbits]})

// placeholder for a lookahead routine on the buffer
let lookaheadBitsName = "lookaheadBits"
let lookaheadBits nbits typeName = Callback({name=lookaheadBitsName; args=[Literal({lit=Ascii; value=typeName})
                                                                           Variable(bufferVar)
                                                                           Variable(cursorVar)
                                                                           nbits]})

let skipBits nbits = Callback({name="skipBits"; args = [Variable(cursorVar);nbits]})