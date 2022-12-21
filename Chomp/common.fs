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