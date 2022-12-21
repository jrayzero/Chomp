module Chomp.common

open AST
open visitor

let mutable nextId = 0

let uniqueVar prefix =
    let prefix =   
        match prefix with
            | Some(pref) -> pref
            | None -> "__x"
    let name = sprintf "%s%d" prefix nextId
    nextId <- nextId + 1
    {name={levels=[name]}}
    
let binaryStringToDec (s:string) =
    let mutable binary: int64 = 0
    let mutable idx = 0
    for c in s do
        if c = '1' then
            let v = int64(1) <<< idx
            binary <- binary + v
        idx <- idx + 1
    binary
    
let asciiStringToDec (s:string) =
    let mutable ascii: int64 = 0
    for c in s do
        let cx = int64(c)
        ascii <- (ascii ||| cx)
        ascii <- ascii <<< 8
    ascii    

type GatherConstants() =
    inherit ConstVisitor()
    
    member val constants = System.Collections.Generic.Dictionary<string,literal>()
    
    static member pass x =
        let this = GatherConstants()
        this.visitProgram x
        this.constants
    
    override this.visitElement x =
        match x with
            | Constant(name,lit) -> this.constants[name] <- lit
            | _ -> base.visitElement x