module AST.builtins

open AST

let bufferName = "buffer"
let bufferVar = {name={levels=[bufferName]}}

let cursorName = "cursor"
let cursorVar = {name={levels=[cursorName]}}

let stopName = "stop"
let stopVar = {name={levels=[stopName]}}

let syntaxParser name = Callback({name="syntax_parse"; args=[Literal({lit=Ascii; value=name})]})

let templateParser name bindings =
    let args = Literal({lit=Ascii; value=name}) :: bindings
    Callback({name="template_parse"; args=args})

let exists nbits = Callback({name="exists"; args=[Variable(cursorVar);Variable(stopVar);nbits]})

let fatal msg = Callback({name="fatal"; args=[Literal({lit=Ascii; value=msg})]})

// placeholder for a parsing routine on the buffer
let parseBitsName = "parseBits"
let parseBits nbits = Callback({name=parseBitsName; args=[Variable(bufferVar);Variable(cursorVar);nbits]})

// placeholder for a lookahead routine on the buffer
let lookaheadBitsName = "lookaheadBits"
let lookaheadBits nbits = Callback({name=lookaheadBitsName; args=[Variable(bufferVar);Variable(cursorVar);nbits]})

let skipBits nbits = Callback({name="skipBits"; args = [Variable(cursorVar);nbits]})