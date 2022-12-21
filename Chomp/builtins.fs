module Chomp.builtins

open AST

let bufferName = "buffer"
let bufferVar = {name={levels=[bufferName]}}

let cursorName = "cursor"
let cursorVar = {name={levels=[cursorName]}}

let syntaxFactory name = Callback({name="syntax_factory"; args=[Literal({lit=Ascii; value=name})]})
let syntaxParser factory = Callback({name="syntax_parse"; args=[factory]})
let syntaxParserFactory name = syntaxParser (syntaxFactory name)

let templateFactory name bindings =
    let args = Literal({lit=Ascii; value=name}) :: bindings
    Callback({name="template_factory"; args=args})
let templateParser factory = Callback({name="template_parse"; args=[factory]})
let templateParserFactory name bindings = templateParser (templateFactory name bindings)

let exists nbits = Callback({name="exists"; args=[Variable(bufferVar);Variable(cursorVar);nbits]})

let fatal msg = Callback({name="fatal"; args=[Literal({lit=Ascii; value=msg})]})

// placeholder for a parsing routine on the buffer
let parseBits nbits = Callback({name="parseBits"; args=[Variable(bufferVar);Variable(cursorVar);nbits]})