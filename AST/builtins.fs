module AST.builtins

open AST

// TODO make sure to create separate names for things

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

let moreData() = Callback({name="moreData"; args=[Variable(cursorVar);Variable(stopVar)]})

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

let curBuffer() = Callback({name="curBuffer"; args = [Variable(bufferVar)]})
let curCursor() = Callback({name="curCursor"; args = [Variable(bufferVar)]})
let curStop() = Callback({name="curStop"; args = [Variable(bufferVar)]})

let isInternal name =
    name = syntaxParseName || name = templateParseName || name = "exists" || name = "moreData"
            || name = "fatal" || name = parseBitsName || name = lookaheadBitsName || name = "skipBits"
            || name = "curBuffer" || name = "curCursor" || name = "curStop" 